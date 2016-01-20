CREATE OR REPLACE PACKAGE BODY PA_ROWGENERATOR IS
  /* ------------------------------------------------------------------------
  -- This is the new RowType-Generator
  -- a generator for pl/sql object types which represent table data

  -- Copyright (C) 2016 Joh. Berenberg, Gossler & Co. KG

  -- This program is licenced under GPL 2.0 licence. For further information 
  -- see the package specification
  ------------------------------------------------------------------------ */

  /* ------------------------------------------------------------------------
  -- record types for internal use
  ------------------------------------------------------------------------ */
  -- specification
  TYPE PK_REC is RECORD(TABLE_NAME  ALL_CONS_COLUMNS.TABLE_NAME%TYPE, 
                        COLUMN_NAME ALL_CONS_COLUMNS.COLUMN_NAME%TYPE,
                        POSITION    ALL_CONS_COLUMNS.POSITION%TYPE,
                        STATUS      ALL_CONSTRAINTS.STATUS%TYPE,
                        OWNER       ALL_CONSTRAINTS.OWNER%TYPE);
  TYPE PK_REC_TAB IS TABLE OF PK_REC;
  
  TYPE COL_REC IS RECORD(COLUMN_NAME    ALL_TAB_COLS.COLUMN_NAME%TYPE,
                         DATA_SCALE     ALL_TAB_COLS.DATA_SCALE%TYPE,
                         DATA_TYPE      ALL_TAB_COLS.DATA_TYPE%TYPE,
                         DATA_TYPE_2    ALL_TAB_COLS.DATA_TYPE%TYPE,
                         DATA_LENGTH    ALL_TAB_COLS.DATA_LENGTH%TYPE,
                         DATA_LENGTH_2  ALL_TAB_COLS.DATA_LENGTH%TYPE,
                         DATA_PRECISION ALL_TAB_COLS.DATA_PRECISION%TYPE,
                         NULLABLE       ALL_TAB_COLS.NULLABLE%TYPE,
                         VIRTUAL_COLUMN ALL_TAB_COLS.VIRTUAL_COLUMN%TYPE,
                         DATA_DEFAULT   ALL_TAB_COLS.DATA_DEFAULT%TYPE);
  TYPE COL_REC_TAB IS TABLE OF COL_REC;
  
  TYPE IND_REC IS RECORD(INDEX_NAME ALL_INDEXES.INDEX_NAME%TYPE);
  TYPE IND_REC_TAB IS TABLE OF IND_REC;
  
  TYPE U_IND_REC IS RECORD(INDEX_NAME      ALL_INDEXES.INDEX_NAME%TYPE,
                           COLUMN_NAME_P4U VARCHAR2(4000),
                           COLUMN_NAME_W4U VARCHAR2(4000),
                           DATA_TYPE       ALL_TAB_COLS.DATA_TYPE%TYPE);
  TYPE U_IND_REC_TAB IS TABLE OF U_IND_REC;

  -- declaration
  rec_all_cols     PA_ROWGENERATOR.COL_REC_TAB ;
  rec_primary_key  PA_ROWGENERATOR.PK_REC_TAB;
  rec_pk_col_names PA_ROWGENERATOR.TABLE_VARCHAR2;
  rec_ind          PA_ROWGENERATOR.IND_REC_TAB;
  rec_u_ind        PA_ROWGENERATOR.U_IND_REC_TAB;

  /* ------------------------------------------------------------------------
  -- forward declaration for internal functions
  ------------------------------------------------------------------------ */
  FUNCTION GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER IN            VARCHAR2,
                                   IN_TABLE_NAME  IN            VARCHAR2,
                                   IN_INDEX_NAME  IN            VARCHAR2,
                                   IN_COLS        IN OUT NOCOPY PA_ROWGENERATOR.COL_REC_TAB,
                                   IN_ACTION      IN            VARCHAR2) RETURN VARCHAR2;

  FUNCTION GET_IN_WHERE4UINDEX(IN_TABLE_OWNER IN            VARCHAR2,
                               IN_TABLE_NAME  IN            VARCHAR2,
                               IN_INDEX_NAME  IN            VARCHAR2,
                               IN_COLS        IN OUT NOCOPY PA_ROWGENERATOR.COL_REC_TAB) RETURN VARCHAR2;                                   

  FUNCTION ADD_DISTINCT_REC(IN_LIST IN PA_ROWGENERATOR.COL_REC_TAB, 
                            IN_REC  IN PA_ROWGENERATOR.COL_REC) RETURN PA_ROWGENERATOR.COL_REC_TAB;

  FUNCTION ADD_GENERATOR_PARAMETER_DOC(IN_TABLE_NAME   IN VARCHAR2,
                                       IN_OWNER        IN VARCHAR2,
                                       IN_ROW_ABBR     IN VARCHAR2,
                                       IN_TAB_ROW_ABBR IN VARCHAR2,
                                       IN_WITH_TAB_ROW IN BOOLEAN,
                                       IN_READONLY     IN BOOLEAN,
                                       IN_IGNORE_LIST  IN PA_ROWGENERATOR.TABLE_VARCHAR2) RETURN VARCHAR2; 

  /* ------------------------------------------------------------------------
  -- internal constants
  ------------------------------------------------------------------------ */
  c_version          CONSTANT VARCHAR2(30) := 'Version: 4.1'; --Generator Version
  c_list_parameter   CONSTANT VARCHAR2(30) := 'LIST_PARAMETER';
  c_list_methodename CONSTANT VARCHAR2(30) := 'METHOD_NAME';
  c_list_call_method CONSTANT VARCHAR2(30) := 'CALL_METHOD';
  c_list_param       CONSTANT VARCHAR2(30) := 'PARM_FUNCTION';

  /* ------------------------------------------------------------------------
  -- internal ATTRIBUTES
  ------------------------------------------------------------------------ */
  b_init_loaded_parameter4uindex BOOLEAN := FALSE;

  /* ------------------------------------------------------------------------
  -- collecting all needed information about the tables' primary key

  -- date   : 04.01.2016
  -- author : Danny Frederich
  ------------------------------------------------------------------------ */
  PROCEDURE SET_PK_REC(IN_OWNER         IN VARCHAR2,
                       IN_TABLE_NAME    IN VARCHAR2,
                       IN_SEQUENCE_NAME IN VARCHAR2)
  IS
  BEGIN
    SELECT cols.table_name, 
           cols.column_name, 
           cols.position, 
           cons.status, 
           cons.owner
      BULK COLLECT INTO rec_primary_key
      FROM ALL_CONSTRAINTS cons                          
      JOIN ALL_CONS_COLUMNS cols on (cons.owner           = cols.owner      AND 
                                     cons.table_name      = cols.table_name AND 
                                     cons.constraint_name = cols.constraint_name)
      JOIN ALL_SEQUENCES als on (als.SEQUENCE_OWNER = cons.owner AND 
                                 als.SEQUENCE_NAME  = IN_SEQUENCE_NAME)
     WHERE cons.owner           = IN_OWNER
       AND cons.table_name      = IN_TABLE_NAME
       AND cons.constraint_type = 'P'
     ORDER BY cols.table_name, cols.position;              

    rec_pk_col_names := PA_ROWGENERATOR.TABLE_VARCHAR2();
    for i in rec_primary_key.first..rec_primary_key.last
    loop
      rec_pk_col_names.extend();
      rec_pk_col_names(i) := rec_primary_key(i).column_name;
    end loop;

  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END SET_PK_REC;

  /* ------------------------------------------------------------------------
  -- collecting all needed information about the tables' columns

  -- date   : 04.01.2016
  -- author : Danny Frederich
  ------------------------------------------------------------------------ */
  PROCEDURE SET_COL_REC(IN_OWNER      IN VARCHAR2,
                        IN_TABLE_NAME IN VARCHAR2)
  IS
  BEGIN
    SELECT ATC.COLUMN_NAME,
           ATC.DATA_SCALE,
           ATC.DATA_TYPE,
           CASE
             WHEN ATC.DATA_TYPE = 'TIMESTAMP(6)' THEN
               'TIMESTAMP'
             WHEN ATC.DATA_TYPE = 'LONG' THEN
                'CLOB'
             WHEN ATC.DATA_TYPE like '%ROWID' THEN
                'VARCHAR'
             WHEN ATC.DATA_TYPE = 'INTERVAL YEAR(2) TO MONTH' THEN
                'INTERVAL YEAR TO MONTH'
             WHEN ATC.DATA_TYPE = 'INTERVAL DAY(2) TO SECOND(6)' THEN
                'INTERVAL DAY TO SECOND'
             ELSE
                ATC.DATA_TYPE
           END AS DATA_TYPE_2,    
           ATC.DATA_LENGTH,
           CASE
             WHEN ATC.DATA_TYPE = 'ROWID' THEN
               4000
             ELSE
               ATC.DATA_LENGTH
           END AS DATA_LENGTH_2,
           ATC.DATA_PRECISION,
           ATC.NULLABLE,
           ATC.VIRTUAL_COLUMN,
           ATC.DATA_DEFAULT
      BULK COLLECT INTO rec_all_cols
      FROM ALL_TAB_COLS ATC
      WHERE ATC.HIDDEN_COLUMN = 'NO'
        AND ATC.OWNER         = IN_OWNER
        AND ATC.TABLE_NAME    = IN_TABLE_NAME;

  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END SET_COL_REC;

  /* ------------------------------------------------------------------------
  -- initially load record for GET_IN_PARAMETER4UINDEX
       
  -- date   : 06.01.2016
  -- author : Danny Frederich
  ------------------------------------------------------------------------ */
  PROCEDURE SET_U_IND_REC(IN_TABLE_OWNER IN VARCHAR2,
                          IN_TABLE_NAME  IN VARCHAR2,
                          IN_COLS        IN OUT NOCOPY PA_ROWGENERATOR.COL_REC_TAB)
  IS
  BEGIN
    rec_u_ind := PA_ROWGENERATOR.U_IND_REC_TAB();
    FOR IND_LOOP IN rec_ind.first..rec_ind.last 
    LOOP
      FOR REC_LOOP IN (SELECT COALESCE(DBMS_LOB.substr(GET_EXPRESSION(IN_TABLE_NAME      => IN_TABLE_NAME,
                                                                      IN_TABLE_OWNER     => IN_TABLE_OWNER,
                                                                      IN_INDEX_NAME      => rec_ind(IND_LOOP).INDEX_NAME,
                                                                      IN_COLUMN_POSITION => AIE.COLUMN_POSITION),
                                                       '4000'),
                                       '"'||AIC.COLUMN_NAME || '"') as COLUMN_NAME_P4U,
                              COALESCE(GET_EXPRESSION(IN_TABLE_NAME      => IN_TABLE_NAME,
                                                      IN_TABLE_OWNER     => IN_TABLE_OWNER,
                                                      IN_INDEX_NAME      => rec_ind(IND_LOOP).INDEX_NAME,
                                                      IN_COLUMN_POSITION => AIE.COLUMN_POSITION),
                                       '"'||AIC.COLUMN_NAME || '"') as COLUMN_NAME_W4U,
                              CASE
                                WHEN ATC.DATA_TYPE = 'TIMESTAMP(6)' THEN
                                  'TIMESTAMP'
                                WHEN ATC.DATA_TYPE = 'LONG' THEN
                                  'CLOB'
                                WHEN ATC.DATA_TYPE like '%ROWID' THEN
                                  'VARCHAR'
                                WHEN ATC.DATA_TYPE = 'INTERVAL YEAR(2) TO MONTH' THEN
                                  'INTERVAL YEAR TO MONTH'
                                WHEN ATC.DATA_TYPE = 'INTERVAL DAY(2) TO SECOND(6)' THEN
                                  'INTERVAL DAY TO SECOND'
                                ELSE
                                ATC.DATA_TYPE
                              END AS DATA_TYPE                             
                         FROM ALL_IND_COLUMNS AIC
                    LEFT JOIN ALL_TAB_COLS ATC ON (ATC.OWNER       = AIC.TABLE_OWNER AND 
                                                   ATC.TABLE_NAME  = AIC.TABLE_NAME  AND 
                                                   AIC.COLUMN_NAME = ATC.COLUMN_NAME)
                    LEFT JOIN ALL_IND_EXPRESSIONS AIE ON (AIC.COLUMN_POSITION = AIE.COLUMN_POSITION AND 
                                                          AIE.INDEX_NAME      = AIC.INDEX_NAME) 
                        WHERE AIC.TABLE_OWNER = IN_TABLE_OWNER
                          AND AIC.TABLE_NAME  = IN_TABLE_NAME
                          AND AIC.INDEX_NAME  = rec_ind(IND_LOOP).INDEX_NAME
                    ORDER BY AIC.COLUMN_POSITION)
      LOOP
        rec_u_ind.extend(1);
        rec_u_ind(rec_u_ind.last).INDEX_NAME      := rec_ind(IND_LOOP).INDEX_NAME;
        rec_u_ind(rec_u_ind.last).COLUMN_NAME_P4U := rec_loop.COLUMN_NAME_P4U;
        rec_u_ind(rec_u_ind.last).COLUMN_NAME_W4U := rec_loop.COLUMN_NAME_W4U;
        rec_u_ind(rec_u_ind.last).DATA_TYPE       := rec_loop.data_type;
      END LOOP;
    END LOOP;
    b_init_loaded_parameter4uindex := true;  
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END SET_U_IND_REC;

  /* ------------------------------------------------------------------------
  -- collecting all needed information about the tables' indexes

  -- date   : 04.01.2016
  -- author : Danny Frederich
  ------------------------------------------------------------------------ */
  PROCEDURE SET_IND_REC(IN_OWNER       IN VARCHAR2,
                        IN_TABLE_NAME  IN VARCHAR2,
                        IN_IGNORE_LIST IN PA_ROWGENERATOR.TABLE_VARCHAR2)
  IS
   rec_ind_buffer PA_ROWGENERATOR.IND_REC_TAB;
  BEGIN

      SELECT DISTINCT AI.INDEX_NAME
        BULK COLLECT INTO rec_ind
        FROM ALL_INDEXES AI
        JOIN ALL_IND_COLUMNS AIC ON (AI.INDEX_NAME = AIC.INDEX_NAME)
        LEFT OUTER JOIN ALL_TAB_COLS DTA ON (DTA.OWNER       = AIC.TABLE_OWNER AND 
                                             DTA.TABLE_NAME  = AIC.TABLE_NAME  AND 
                                             AIC.COLUMN_NAME = DTA.COLUMN_NAME)
       WHERE AI.UNIQUENESS      = 'UNIQUE'
         AND AI.TABLE_OWNER     = IN_OWNER
         AND AI.TABLE_NAME      = IN_TABLE_NAME
         AND DTA.VIRTUAL_COLUMN = 'NO'
       ORDER BY AI.INDEX_NAME DESC;

    IF IN_IGNORE_LIST IS NOT NULL
    THEN
      rec_ind_buffer := PA_ROWGENERATOR.IND_REC_TAB();
      for ind_loop in rec_ind.first..rec_ind.last
      loop
        if rec_ind(ind_loop).INDEX_NAME NOT MEMBER OF IN_IGNORE_LIST
        then
          rec_ind_buffer.extend;
          rec_ind_buffer(ind_loop).INDEX_NAME := rec_ind(ind_loop).INDEX_NAME;
        end if;
      end loop;
      rec_ind.delete();
      rec_ind := rec_ind_buffer;
    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END SET_IND_REC;  

  /* ------------------------------------------------------------------------
  -- append (standard) exception block

  -- date   : 04.01.2016
  -- author : Danny Frederich
  ------------------------------------------------------------------------ */
  PROCEDURE APPEND_EXCEPTION(IN_OUT_TYPE IN OUT CLOB,
                             IN_METHOD   IN VARCHAR2 DEFAULT NULL)
  IS 
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  EXCEPTION' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHEN OTHERS THEN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    RAISE;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  END'||CASE WHEN IN_METHOD IS NOT NULL THEN ' '||IN_METHOD END ||';' || chr(10));
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END APPEND_EXCEPTION;

  /* ------------------------------------------------------------------------
  -- build insert trigger with standard columns 
     
  -- date   : 22.12.2015
  -- author : Andreas Bachmann
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_INSERT_TRIGGER(IN_TABLE_NAME    IN VARCHAR2,
                                  IN_OWNER         IN VARCHAR2,
                                  IN_SEQUENCE_NAME IN VARCHAR2,
                                  IN_OUT_TYPE      IN OUT CLOB)
  IS
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => 'CREATE OR REPLACE TRIGGER TBI$' || IN_TABLE_NAME || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  BEFORE INSERT ON ' || IN_OWNER || '.' || IN_TABLE_NAME || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  FOR EACH ROW' || CHR(10));

    FOR pk IN rec_primary_key.first..rec_primary_key.last
    LOOP 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => 'WHEN ( NEW.' || rec_primary_key(pk).column_name || ' IS NULL ) ' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => 'BEGIN ' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  :NEW.'|| rec_primary_key(pk).column_name  ||' := '|| IN_SEQUENCE_NAME||'.NEXTVAL;' || CHR(10)); 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  :NEW.'||v_created_user||'  := SYS_CONTEXT(''USERENV'',''OS_USER'');' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  :NEW.'||v_created_date||'  := SYSDATE;' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => 'END;' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '/' ||CHR(10));
      EXIT;
    END LOOP;
  EXCEPTION 
    WHEN OTHERS THEN
      RAISE;
  END CREATE_INSERT_TRIGGER;                                  
     
  /* ------------------------------------------------------------------------
  --  build update trigger with standard columns 
      
  --  date   : 22.12.2015
  --  author : Andreas Bachmann  
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_UPDATE_TRIGGER(IN_TABLE_NAME IN VARCHAR2,
                                  IN_OWNER      IN VARCHAR2,
                                  IN_OUT_TYPE   IN OUT CLOB)
  IS
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => 'CREATE OR REPLACE TRIGGER TBU$' || IN_TABLE_NAME || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  BEFORE UPDATE ON ' || IN_OWNER || '.' || IN_TABLE_NAME || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  FOR EACH ROW' || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => 'BEGIN ' || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  :NEW.'|| v_changed_user ||'    := SYS_CONTEXT(''USERENV'',''OS_USER'');' || CHR(10)); 
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  :NEW.'|| v_changed_date ||'    := SYSDATE;' || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '  :NEW.'|| v_changed_counter ||' := :OLD.'||v_changed_counter||' + 1;' || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => 'END;' || CHR(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob  => '/' ||CHR(10));
  END;

  /* ------------------------------------------------------------------------
  -- build the Row-type-specification.
  
  -- date   : 21.09.2015
  -- author : Andreas Bachmann
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_SPEC(IN_TABLE_NAME   IN VARCHAR2,
                        IN_OWNER        IN VARCHAR2,  
                        IN_HAVE_CHANGED IN BOOLEAN,
                        IN_MAX_COL_SIZE IN NUMBER,
                        IN_TABLE_ABBR   IN VARCHAR2,
                        IN_TAB_ROW_ABBR IN VARCHAR2,
                        IN_WITH_TAB_ROW IN BOOLEAN,
                        IN_READONLY     IN BOOLEAN,
                        IN_IGNORE_LIST  IN PA_ROWGENERATOR.TABLE_VARCHAR2,
                        IN_OUT_TYPE     IN OUT CLOB) 
  IS
    b_FIRST           BOOLEAN;      
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter: ' || CHR(10) || 'IN_TABLE_NAME        = ' || IN_TABLE_NAME 
                           || CHR(10) || 'IN_OWNER             = ' || IN_OWNER                           
                           || CHR(10) || 'IN_HAVE_CHANGED      = ' || CASE WHEN IN_HAVE_CHANGED THEN 'TRUE' ELSE 'FALSE' END 
                           || CHR(10) || 'IN_MAX_COL_SIZE      = ' || TO_CHAR(IN_MAX_COL_SIZE)                            
                           || CHR(10) || 'IN_TABLE_ABBR        = ' || IN_TABLE_ABBR
                           || CHR(10) || 'IN_TAB_ROW_ABBR      = ' || IN_TAB_ROW_ABBR
                           || CHR(10) || 'IN_WITH_TAB_ROW      = ' || CASE WHEN IN_WITH_TAB_ROW THEN 'TRUE' ELSE 'FALSE' END
                           || CHR(10) || 'IN_READONLY          = ' || CASE WHEN IN_READONLY THEN 'TRUE' ELSE 'FALSE' END
                           || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT
                           ;
    END;
  BEGIN
    --header
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                    src_lob  => 'CREATE OR REPLACE TYPE ' || IN_OWNER || '.ROW_' || IN_TABLE_ABBR || ' FORCE AS OBJECT' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '(');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- object oriented ROWTYPE for ' || IN_TABLE_ABBR || ' table' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- This Type was created using the OS-RowGenerator provided by Berenberg.' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- The generator is licensed under GPL 2.0 licence.' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- For further details see package specification of the generator.' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    -- generator parameter
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ADD_GENERATOR_PARAMETER_DOC(IN_TABLE_NAME   => IN_TABLE_NAME,
                                                                                    IN_OWNER        => IN_OWNER,                                                                                         
                                                                                    IN_ROW_ABBR     => IN_TABLE_ABBR,
                                                                                    IN_TAB_ROW_ABBR => IN_TAB_ROW_ABBR,
                                                                                    IN_WITH_TAB_ROW => IN_WITH_TAB_ROW,
                                                                                    IN_READONLY     => IN_READONLY,                                                                                              
                                                                                    IN_IGNORE_LIST  => IN_IGNORE_LIST));
    --attributes
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- attributes' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    ' || rpad('OBJECT_TYPE_NAME', IN_MAX_COL_SIZE + 1)||' VARCHAR2(100)' || chr(10));
  
    b_FIRST := TRUE;
    FOR COLUMN IN rec_all_cols.first..rec_all_cols.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , ');
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , ');
      END IF;
      IF rec_all_cols(COLUMN).DATA_TYPE_2 = 'NUMBER' 
        AND rec_all_cols(COLUMN).Data_Precision IS NOT NULL 
        AND (rec_all_cols(COLUMN).DATA_SCALE IS NULL or rec_all_cols(COLUMN).DATA_SCALE = 0 ) 
      THEN
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                            src_lob  => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 || '(' || rec_all_cols(column).DATA_PRECISION || ')' ||
                           chr(10));
      ELSIF rec_all_cols(column).DATA_TYPE_2 = 'NUMBER' 
        AND rec_all_cols(column).Data_Precision IS NOT NULL 
        AND rec_all_cols(column).DATA_SCALE IS NOT NULL 
        AND rec_all_cols(column).DATA_SCALE > 0  
      THEN
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                            src_lob  => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 || '(' || rec_all_cols(column).DATA_PRECISION || ','|| rec_all_cols(column).DATA_SCALE ||')' ||chr(10));  
      ELSIF rec_all_cols(column).DATA_TYPE_2 = 'NUMBER'
        AND rec_all_cols(column).DATA_PRECISION IS NULL 
        AND rec_all_cols(column).DATA_SCALE IS NULL 
      THEN 
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                            src_lob  => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 ||
                           chr(10));                    
      ELSIF rec_all_cols(column).DATA_TYPE_2 IN ('DATE', 'XMLTYPE', 'BFILE', 'BINARY_FLOAT', 'BINARY_DOUBLE') OR rec_all_cols(column).DATA_TYPE_2 like '%LOB' THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 || chr(10));
      ELSIF rec_all_cols(column).DATA_TYPE_2 = 'TIMESTAMP' THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                        src_lob  => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 || '(' || rec_all_cols(column).DATA_SCALE || ')' || chr(10));
      ELSIF rec_all_cols(column).DATA_TYPE_2 IN ( 'TIMESTAMP(6) WITH TIME ZONE','TIMESTAMP(6) WITH LOCAL TIME ZONE') THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 || chr(10));
      ELSIF rec_all_cols(column).DATA_TYPE_2 like 'INTERVAL%' THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 || chr(10));
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                        src_lob  => rpad(rec_all_cols(column).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' ' || rec_all_cols(column).DATA_TYPE_2 || '(' || rec_all_cols(column).DATA_LENGTH || ')' || chr(10));
      END IF;
    
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
  
    --constructors
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- define constructors' || chr(10));

    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , CONSTRUCTOR FUNCTION ROW_' || IN_TABLE_ABBR || ' RETURN SELF AS RESULT' || chr(10));

    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , CONSTRUCTOR FUNCTION ROW_' || IN_TABLE_ABBR || '(');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') RETURN SELF AS RESULT' || chr(10));
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
  
    --members
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- define member functions' || chr(10));
    --ROW_EXISTS
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      b_FIRST := TRUE;
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER FUNCTION ROW_EXISTS(');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER));        
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') RETURN BOOLEAN' || chr(10));
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    
     --members
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- define static member functions' || chr(10));
    --STATIC
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP    
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , STATIC FUNCTION EXIST (');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER)); 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') RETURN BOOLEAN' || chr(10));
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
  
    --COMPARE
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                    src_lob  => '  , MEMBER FUNCTION COMPARE(in_type1 ' || IN_OWNER || '.ROW_' || IN_TABLE_ABBR || ', in_type2 ' || IN_OWNER || '.ROW_' || IN_TABLE_ABBR || ') RETURN INTEGER' || chr(10));                  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- define member procedures' || chr(10));
  
    /* at readonly , create no DML pperations */
    IF IN_READONLY = FALSE THEN
        --ROW_INSERT
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_' || 'INSERT' || chr(10));
      
        --ROW_UPDATE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_' || 'UPDATE' || chr(10));
      
        --ROW_MERGE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_' || 'MERGE' || chr(10));
      
        --ROW_SAVE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_' || 'SAVE' || chr(10));
      
        --ROW_DELETE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_' || 'DELETE' || chr(10));
    END IF;
    
    --ROW_SELECT
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      b_FIRST := TRUE;
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_SELECT(');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER)); 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ')' || chr(10));
    END LOOP;
  
    --ROW_INIT  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_' || 'INIT' || chr(10));
  
    --ROW_DEFAULT
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_' || 'DEFAULT' || chr(10));
  
    --ROW_LOCK
    if IN_HAVE_CHANGED THEN
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_LOCK' || chr(10));
    END IF;
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER PROCEDURE ROW_LOCK(');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER)); 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ')' || chr(10));
    END LOOP;
    
    -- TO_CLOB
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  , MEMBER FUNCTION TO_CLOB RETURN CLOB'|| chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') NOT FINAL' || chr(10));
    
  EXCEPTION
    WHEN OTHERS THEN      
      RAISE;
  END CREATE_SPEC;
  
  /* ------------------------------------------------------------------------
  -- Create Constructor Function

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_CONSTRUCTOR(IN_TABLE_NAME  IN VARCHAR2,
                               IN_OWNER       IN VARCHAR2,
                               IN_TABLE_ABBR  IN VARCHAR2,
                               IN_IGNORE_LIST IN PA_ROWGENERATOR.TABLE_VARCHAR2,
                               IN_OUT_TYPE    IN OUT CLOB) 
  IS
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_TABLE_ABBR     = ' || IN_TABLE_ABBR
                          || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT()
                          ;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'CREATE OR REPLACE TYPE BODY ' || IN_OWNER || '.ROW_' || IN_TABLE_ABBR || ' IS' || chr(10) || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- constructors' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  CONSTRUCTOR FUNCTION ROW_' || IN_TABLE_ABBR || ' RETURN SELF AS RESULT' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10)); 
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELF.OBJECT_TYPE_NAME  := ''' || IN_OWNER || '.ROW_' || IN_TABLE_NAME || ''';' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELF.ROW_DEFAULT();' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    RETURN;' || chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE);
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));
    
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  CONSTRUCTOR FUNCTION ROW_' || IN_TABLE_ABBR || '(');      
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') RETURN SELF AS RESULT' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));      
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELF.OBJECT_TYPE_NAME  := ''' || IN_OWNER || '.ROW_' || IN_TABLE_NAME || ''';' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELF.ROW_SELECT(');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_CALL_METHOD));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ');' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    RETURN;' || chr(10));
      PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE);
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));
    END LOOP;
  
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_CONSTRUCTOR;
 
 /* ------------------------------------------------------------------------
  -- Create Exists-Function

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_EXISTS(IN_TABLE_NAME  IN VARCHAR2,
                          IN_OWNER       IN VARCHAR2,  
                          IN_IGNORE_LIST IN PA_ROWGENERATOR.TABLE_VARCHAR2,
                          IN_OUT_TYPE    IN OUT CLOB) 
  IS
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME        = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER             = ' || IN_OWNER 
                          || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT()
                          ;
    END;
  BEGIN
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER FUNCTION ROW_EXISTS(');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') RETURN BOOLEAN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));    
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    v_count  PLS_INTEGER;' || chr(10));
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '    FUNCTION parm RETURN CLOB IS BEGIN ' || chr(10));
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN ''PARAMETER: ''' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAM)); 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '        || CHR(10) || '' SELF:''  || SELF.TO_CLOB();' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' END parm;' || chr(10));                                                                                                        
      
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));    
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELECT COUNT(*)' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    INTO v_count' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FROM ' || IN_OWNER || '.' || IN_TABLE_NAME || '' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHERE ');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => GET_IN_WHERE4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                              IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                              IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                              IN_COLS        => rec_all_cols));      
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ';' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    RETURN (v_count <> 0);' || chr(10));
      PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_EXISTS');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));     
    END LOOP;
  
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_EXISTS;

   /* ------------------------------------------------------------------------
  -- Create static Exist-Function

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_STATIC_EXIST(IN_TABLE_NAME  IN VARCHAR2,
                                IN_OWNER       IN VARCHAR2,
                                IN_IGNORE_LIST IN PA_ROWGENERATOR.TABLE_VARCHAR2,
                                IN_OUT_TYPE    IN OUT CLOB) 
  IS    
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT()
                          ;
    END;
  BEGIN
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  STATIC FUNCTION EXIST (');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') RETURN BOOLEAN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));   
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    v_count  PLS_INTEGER;' || chr(10));
       -- define parm function
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '    FUNCTION parm RETURN CLOB IS BEGIN ' || chr(10));
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN ''PARAMETER: ''' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAM)); 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ';' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' END parm;' || chr(10));     
        
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELECT COUNT(*)' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    INTO v_count' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FROM ' || IN_OWNER || '.' || IN_TABLE_NAME || '' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHERE ');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => GET_IN_WHERE4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                              IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                              IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                              IN_COLS        => rec_all_cols)); 

      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ';' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    RETURN (v_count <> 0);' || chr(10));
      PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'EXIST');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));
    END LOOP;
  
  EXCEPTION
    WHEN OTHERS THEN     
      RAISE;
  END CREATE_STATIC_EXIST;

  /* ------------------------------------------------------------------------
  -- Create Compare-Funktion

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_COMPARE(IN_TABLE_NAME   IN VARCHAR2,
                           IN_OWNER        IN VARCHAR2,
                           IN_MAX_COL_SIZE IN NUMBER,
                           IN_TABLE_ABBR   IN VARCHAR2,
                           IN_OUT_TYPE     IN OUT CLOB)
  IS
    b_FIRST BOOLEAN;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME   = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER        = ' || IN_OWNER 
                          || CHR(10) || 'IN_MAX_COL_SIZE = ' || IN_MAX_COL_SIZE 
                          || CHR(10) || 'IN_TABLE_ABBR   = ' || IN_TABLE_ABBR;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- member functions' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                    src_lob  => '  MEMBER FUNCTION COMPARE(in_type1 ' || IN_OWNER || '.ROW_' || IN_TABLE_ABBR || ', in_type2 ' || IN_OWNER || '.ROW_' || IN_TABLE_ABBR || ') RETURN INTEGER' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                    src_lob  => '    type1         ROW_' || IN_TABLE_ABBR || ' := TREAT(in_type1 AS ROW_' || IN_TABLE_ABBR || ');' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                    src_lob  => '    type2         ROW_' || IN_TABLE_ABBR || ' := TREAT(in_type2 AS ROW_' || IN_TABLE_ABBR || ');' || chr(10));
    -- define Parm function
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FUNCTION parm RETURN CLOB IS BEGIN ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN ''PARAMETER: ''' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '        || CHR(10) || ''in_type1 = '' || in_type1.TO_CLOB() ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '        || CHR(10) || ''in_type2 = '' || in_type2.TO_CLOB() ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '        || CHR(10) || ''SELF:'' || SELF.TO_CLOB(); ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    END;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
  
  
    FOR rec IN (SELECT ATC.COLUMN_NAME 
                  FROM ALL_TAB_COLS ATC 
                WHERE ATC.DATA_TYPE  = 'BFILE'
                  AND ATC.TABLE_NAME = IN_TABLE_NAME
                  AND ATC.OWNER      = IN_OWNER)
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    DBMS_LOB.open(' || 'type1.' || rec.COLUMN_NAME || ');' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    DBMS_LOB.open(' || 'type2.' || rec.COLUMN_NAME || ');' || chr(10));
      EXIT;
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    IF');
  
    b_FIRST := true;
    FOR COLUMN IN rec_all_cols.first..rec_all_cols.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '   ');
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      AND');
      END IF;
      --when CLOB
      IF upper(rec_all_cols(COLUMN).DATA_TYPE_2) like '%LOB' THEN
        IF upper(rec_all_cols(COLUMN).NULLABLE) = 'N' THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '   DBMS_LOB.COMPARE(type1.' || rec_all_cols(COLUMN).COLUMN_NAME || ', type2.' || rec_all_cols(COLUMN).COLUMN_NAME || ') = 0' || chr(10));
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => ' ( ' || 'DBMS_LOB.COMPARE(type1.' || rec_all_cols(COLUMN).COLUMN_NAME || ', type2.' || rec_all_cols(COLUMN).COLUMN_NAME || ') = 0 OR (type1.' ||
                                      rec_all_cols(COLUMN).COLUMN_NAME || ' IS NULL AND type2.' || rec_all_cols(COLUMN).COLUMN_NAME || ' IS NULL) )' || chr(10));
        END IF;
      
        --when BFILE
      ELSIF upper(rec_all_cols(COLUMN).DATA_TYPE_2) = 'BFILE' THEN
        IF upper(rec_all_cols(COLUMN).NULLABLE) = 'N' THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '   DBMS_LOB.COMPARE(type1.' || rec_all_cols(COLUMN).COLUMN_NAME || ', type2.' || rec_all_cols(COLUMN).COLUMN_NAME || ', 1048576' /* 1MB */
                                      || ') = 0' || chr(10));
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => ' ( ' || 'DBMS_LOB.COMPARE(type1.' || rec_all_cols(COLUMN).COLUMN_NAME || ', type2.' || rec_all_cols(COLUMN).COLUMN_NAME || ', 1048576' ||
                                      ') = 0 OR (type1.' || rec_all_cols(COLUMN).COLUMN_NAME || ' IS NULL AND type2.' || rec_all_cols(COLUMN).COLUMN_NAME || ' IS NULL) )' || chr(10));
        END IF;
        --when XMLTYPE
      ELSIF upper(rec_all_cols(COLUMN).DATA_TYPE_2) = 'XMLTYPE' THEN
        IF upper(rec_all_cols(COLUMN).NULLABLE) = 'N' THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '   DBMS_LOB.COMPARE(type1.' || rec_all_cols(COLUMN).COLUMN_NAME || '.GETCLOBVAL(), type2.' || rec_all_cols(COLUMN).COLUMN_NAME ||
                                      '.GETCLOBVAL()) = 0' || chr(10));
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => ' ( ' || 'DBMS_LOB.COMPARE(type1.' || rec_all_cols(COLUMN).COLUMN_NAME || '.GETCLOBVAL(), type2.' || rec_all_cols(COLUMN).COLUMN_NAME ||
                                      '.GETCLOBVAL()) = 0 OR (type1.' || rec_all_cols(COLUMN).COLUMN_NAME || ' IS NULL AND type2.' || rec_all_cols(COLUMN).COLUMN_NAME || ' IS NULL) )' ||
                                      chr(10));
        END IF;
      ELSE
        IF upper(rec_all_cols(COLUMN).NULLABLE) = 'N' THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '   type1.' || rpad(rec_all_cols(COLUMN).COLUMN_NAME, IN_MAX_COL_SIZE, ' ') || ' = type2.' || rec_all_cols(COLUMN).COLUMN_NAME || chr(10));
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => ' ( type1.' || rpad(rec_all_cols(COLUMN).COLUMN_NAME, IN_MAX_COL_SIZE, ' ') || ' = type2.' ||
                                      rpad(rec_all_cols(COLUMN).COLUMN_NAME, IN_MAX_COL_SIZE, ' ') || ' OR (type1.' || rpad(rec_all_cols(COLUMN).COLUMN_NAME, IN_MAX_COL_SIZE, ' ') ||
                                      ' IS NULL AND type2.' || rpad(rec_all_cols(COLUMN).COLUMN_NAME, IN_MAX_COL_SIZE, ' ') || ' IS NULL) )' || chr(10));
        END IF;
      END IF;
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    THEN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN 0; ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    ELSE' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN 1; ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    END IF;' || chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'COMPARE');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));
  
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_COMPARE;

  /* ------------------------------------------------------------------------
  -- Create Insert-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
 PROCEDURE CREATE_INSERT(IN_TABLE_NAME    IN VARCHAR2,
                          IN_OWNER         IN VARCHAR2,
                          IN_TABLE_ABBR    IN VARCHAR2,
                          IN_SEQUENCE_NAME IN VARCHAR2,
                          IN_OUT_TYPE      IN OUT CLOB) 
  IS   
    b_FIRST BOOLEAN;

    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_TABLE_ABBR    = ' || IN_TABLE_ABBR
                          || CHR(10) || 'IN_SEQUENCE_NAME = ' || IN_SEQUENCE_NAME;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  -- member procedures' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_INSERT' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
   
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                      src_lob  => '    v_' || rec_primary_key(pk).column_name || ' ' || IN_OWNER || '.' || IN_TABLE_NAME || '.' || rec_primary_key(pk).COLUMN_NAME || '%TYPE;' || chr(10));
    END LOOP;

    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
  
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                      src_lob  => '    v_' || rec_primary_key(pk).column_name || ' := ' || 'COALESCE (SELF.' || rec_primary_key(pk).column_name || ',' || IN_SEQUENCE_NAME || '.NEXTVAL);' || chr(10));
    END LOOP;
      
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    INSERT INTO ' || IN_OWNER || '.' || IN_TABLE_NAME || ' A (');
  
    --PK
    b_FIRST := true;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' A.' || rec_primary_key(pk).column_name || chr(10));
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',A.', length(IN_OWNER || IN_TABLE_NAME) + 24, ' ') || rec_primary_key(pk).column_name || chr(10));
      END IF;
    END LOOP;

    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
      THEN
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' A.' || rec_all_cols(COLUMN).column_name || chr(10));
          b_FIRST := false;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',A.', length(IN_OWNER || IN_TABLE_NAME) + 24) || rec_all_cols(COLUMN).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(') VALUES ( ', length(IN_OWNER || IN_TABLE_NAME) + 22, ' '));
    b_FIRST := TRUE;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'v_' || rec_primary_key(pk).column_name || chr(10));
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',v_', length(IN_OWNER || IN_TABLE_NAME) + 24, ' ') || rec_primary_key(pk).column_name || chr(10));
      END IF;
    END LOOP;
  
    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
      THEN
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' SELF.' || rec_all_cols(COLUMN).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',SELF.', length(IN_OWNER || IN_TABLE_NAME) + 27, ' ') || rec_all_cols(COLUMN).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(' ) RETURNING', length(IN_OWNER || IN_TABLE_NAME) + 19, ' ') || chr(10));
  
    b_FIRST := TRUE;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad('A.', length(IN_OWNER || IN_TABLE_NAME) + 24, ' ') || rec_primary_key(pk).column_name || chr(10));
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',A.', length(IN_OWNER || IN_TABLE_NAME) + 24, ' ') || rec_primary_key(pk).column_name || chr(10));
      END IF;
    
    END LOOP;

    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
      THEN  
        IF rec_all_cols(column).DATA_TYPE_2 IN ('XMLTYPE', 'LONG') THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '--' || lpad('A.', length(IN_OWNER || IN_TABLE_NAME) + 22, ' ') || rec_all_cols(column).column_name || '' || chr(10));
        ELSIF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad('A.', length(IN_OWNER || IN_TABLE_NAME) + 24, ' ') || rec_all_cols(column).column_name || '' || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',A.', length(IN_OWNER || IN_TABLE_NAME) + 24, ' ') || rec_all_cols(column).column_name || '' || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad('INTO', length(IN_OWNER || IN_TABLE_NAME) + 19, ' ') || chr(10));
  
    b_FIRST := TRUE;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad('SELF.', length(IN_OWNER || IN_TABLE_NAME) + 27) || rec_primary_key(pk).column_name || chr(10));
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',SELF.', length(IN_OWNER || IN_TABLE_NAME) + 27) || rec_primary_key(pk).column_name || chr(10));
      END IF;
    END LOOP;

    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
      THEN
        IF rec_all_cols(column).DATA_TYPE_2 IN ('XMLTYPE', 'LONG') THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '--' || lpad('SELF.', length(IN_OWNER || IN_TABLE_NAME) + 25, ' ') || rec_all_cols(column).column_name || '' || chr(10));
        ELSIF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad('SELF.', length(IN_OWNER || IN_TABLE_NAME) + 27) || rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(',SELF.', length(IN_OWNER || IN_TABLE_NAME) + 27) || rec_all_cols(column).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => lpad(';', length(IN_OWNER || IN_TABLE_NAME) + 22) || chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_INSERT');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '---------------------------------------------------------------' || chr(10) || chr(10));
    
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_INSERT;


  /* ------------------------------------------------------------------------
  -- Create Update-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_UPDATE(IN_TABLE_NAME    IN VARCHAR2,
                          IN_OWNER         IN VARCHAR2,
                          IN_HAVE_CHANGED  IN BOOLEAN,
                          IN_MAX_COL_SIZE  IN NUMBER,
                          IN_TABLE_ABBR    IN VARCHAR2,
                          IN_SEQUENCE_NAME IN VARCHAR2,
                          IN_OUT_TYPE      IN OUT CLOB)
  IS
    b_FIRST BOOLEAN;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_HAVE_CHANGED  = ' || CASE WHEN IN_HAVE_CHANGED THEN 'TRUE' ELSE 'FALSE' END 
                          || CHR(10) || 'IN_MAX_COL_SIZE  = ' || TO_CHAR(IN_MAX_COL_SIZE) 
                          || CHR(10) || 'IN_TABLE_ABBR    = ' || IN_TABLE_ABBR
                          || CHR(10) || 'IN_SEQUENCE_NAME = ' || IN_SEQUENCE_NAME;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_UPDATE' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));

    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    UPDATE ' || IN_OWNER || '.' || IN_TABLE_NAME || ' A' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SET');
  
    b_FIRST := true;

    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
      THEN
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => ' A.' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE + 1) || ' = SELF.' || rec_all_cols(column).column_name || chr(10));
          b_FIRST := false;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '       ,A.' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE + 1) || ' = SELF.' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHERE ');
  
    b_FIRST := TRUE;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'A.' || rpad(rec_primary_key(pk).column_name, IN_MAX_COL_SIZE + 1) || '= SELF.' || rec_primary_key(pk).column_name || chr(10));        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                        src_lob  => '      AND A.' || rpad(rec_primary_key(pk).column_name, IN_MAX_COL_SIZE + 1) || '= SELF.' || rec_primary_key(pk).column_name || chr(10));
      END IF;
    END LOOP;
  
    IF IN_have_changed THEN
      FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
      LOOP
        IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
           and rec_all_cols(column).COLUMN_NAME like '%'||v_changed_counter||'%'
        THEN
          IF b_FIRST THEN
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                            src_lob  => ' A.' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE + 1) || '= SELF.' || rec_all_cols(column).column_name || chr(10));
            b_FIRST := FALSE;
          ELSE
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                            src_lob  => '      AND A.' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE + 1) || '= SELF.' || rec_all_cols(column).column_name || chr(10));
          END IF;
        END IF;
      END LOOP;
    END IF;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    RETURNING');
  
    b_FIRST := TRUE;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' A.' || rec_primary_key(pk).column_name || chr(10));
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '             ,A.' || rec_primary_key(pk).column_name || chr(10));
      END IF;
    
    END LOOP;
  
    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
      THEN 
        IF rec_all_cols(column).DATA_TYPE_2 IN ('XMLTYPE', 'LONG') THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '--           ,A.' || rec_all_cols(column).column_name || chr(10));
        ELSIF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' A.' || rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '             ,A.' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '         INTO ');
  
    b_FIRST := TRUE;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'SELF.' || rec_primary_key(pk).column_name || chr(10));
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '             ,SELF.' || rec_primary_key(pk).column_name || chr(10));
      END IF;
    END LOOP;

    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
      THEN 
        IF rec_all_cols(column).DATA_TYPE_2 IN ('XMLTYPE', 'LONG') THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '--           ,SELF.' || rec_all_cols(column).column_name || chr(10));
        ELSIF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'SELF.' || rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '             ,SELF.' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '            ;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    IF SQL%ROWCOUNT <> 1 THEN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      RAISE_APPLICATION_ERROR (-20999 ,''record has changed in the meantime'');' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    END IF;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_UPDATE');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '---------------------------------------------------------------' || chr(10) || chr(10));
  
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_UPDATE;

  /* ------------------------------------------------------------------------
  -- Create Merge-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_MERGE(IN_TABLE_NAME    IN VARCHAR2,
                         IN_OWNER         IN VARCHAR2,
                         IN_MAX_COL_SIZE  IN NUMBER,
                         IN_TABLE_ABBR    IN VARCHAR2,
                         IN_SEQUENCE_NAME IN VARCHAR2,
                         IN_OUT_TYPE      IN OUT CLOB) 
  IS   
    b_FIRST BOOLEAN;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_MAX_COL_SIZE  = ' || IN_MAX_COL_SIZE 
                          || CHR(10) || 'IN_TABLE_ABBR    = ' || IN_TABLE_ABBR
                          || CHR(10) || 'IN_SEQUENCE_NAME = ' || IN_SEQUENCE_NAME;
    END;
  
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_MERGE ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));

    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    MERGE INTO ' || IN_OWNER || '.' || IN_TABLE_NAME || ' A' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    USING ( SELECT');
  
    b_FIRST := TRUE;
    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
      THEN
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => ' SELF.' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE + 1) || ' AS ' || rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '                  ,SELF.' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE + 1) || ' AS ' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '            FROM DUAL ) B' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    ON (');
    b_FIRST := TRUE;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    A.' || rec_primary_key(pk).column_name || ' = B.' || rec_primary_key(pk).column_name);
        b_FIRST := FALSE;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '        AND A.' || rec_primary_key(pk).column_name || ' = B.' || rec_primary_key(pk).column_name);
      END IF;
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ')' || chr(10));
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHEN MATCHED THEN UPDATE SET   ');
  
    b_FIRST := true;
    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF rec_pk_col_names
      THEN
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE) || ' = B.' || rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                          src_lob  => '                                  ,' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE) || ' = B.' || rec_all_cols(column).column_name ||
                                      chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHEN NOT MATCHED THEN INSERT (');
  
    b_FIRST := TRUE;
    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
      THEN
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '                                  ,' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '                        ) VALUES (');
  
    b_FIRST := TRUE;
    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF     rec_all_cols(column).VIRTUAL_COLUMN = 'NO'
         and rec_all_cols(column).COLUMN_NAME NOT MEMBER OF t_std_cols
      THEN
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' B.' || rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '                                  ,B.' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END IF;
    END LOOP;
  
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '                                );' || chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_MERGE');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '---------------------------------------------------------------' || chr(10) || chr(10));
    
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_MERGE;

  /* ------------------------------------------------------------------------
  -- Create Save-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_SAVE(IN_TABLE_NAME    IN VARCHAR2,
                        IN_OWNER         IN VARCHAR2,
                        IN_TABLE_ABBR    IN VARCHAR2,
                        IN_SEQUENCE_NAME IN VARCHAR2,
                        IN_OUT_TYPE      IN OUT CLOB) 
  IS    
    b_FIRST BOOLEAN;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_TABLE_ABBR    = ' || IN_TABLE_ABBR
                          || CHR(10) || 'IN_SEQUENCE_NAME = ' || IN_SEQUENCE_NAME;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_SAVE ' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
 
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    IF SELF.ROW_EXISTS(');
    b_FIRST := true;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'IN_' || rec_primary_key(pk).column_name || ' => ' || 'SELF.' || rec_primary_key(pk).column_name);
        b_FIRST := false;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ', IN_' || rec_primary_key(pk).column_name || ' => ' || 'SELF.' || rec_primary_key(pk).column_name);
      END IF;
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ') THEN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      SELF.ROW_UPDATE;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    ELSE' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      SELF.ROW_INSERT;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    END IF;' || chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_SAVE');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '---------------------------------------------------------------' || chr(10) || chr(10));
    
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_SAVE;

  /* ------------------------------------------------------------------------
  -- Create Delete-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_DELETE(IN_TABLE_NAME    IN VARCHAR2,
                          IN_OWNER         IN VARCHAR2,
                          IN_HAVE_CHANGED  IN BOOLEAN,
                          IN_MAX_COL_SIZE  IN NUMBER,
                          IN_TABLE_ABBR    IN VARCHAR2,
                          IN_SEQUENCE_NAME IN VARCHAR2,
                          IN_OUT_TYPE      IN OUT CLOB)
  IS    
    b_FIRST BOOLEAN;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME     = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER          = ' || IN_OWNER 
                          || CHR(10) || 'IN_HAVE_CHANGED = ' || CASE WHEN IN_HAVE_CHANGED THEN 'TRUE' ELSE 'FALSE' END 
                          || CHR(10) || 'IN_MAX_COL_SIZE   = ' || TO_CHAR(IN_MAX_COL_SIZE) 
                          || CHR(10) || 'IN_TABLE_ABBR     = ' || IN_TABLE_ABBR
                          || CHR(10) || 'IN_SEQUENCE_NAME  = ' || IN_SEQUENCE_NAME;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_DELETE' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
 
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    DELETE FROM ' || IN_OWNER || '.' || IN_TABLE_NAME || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHERE ');
  
    b_FIRST := true;
    for pk in rec_primary_key.first..rec_primary_key.last
    LOOP
      IF b_FIRST THEN
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rpad(rec_primary_key(pk).column_name, IN_MAX_COL_SIZE) || ' = SELF.' || rec_primary_key(pk).column_name || chr(10));
        b_FIRST := false;
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '     AND ' || rpad(rec_primary_key(pk).column_name, IN_MAX_COL_SIZE) || ' = SELF.' || rec_primary_key(pk).column_name || chr(10));
      END IF;
    END LOOP;
  
    if IN_have_changed THEN
      FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
      LOOP
        IF rec_all_cols(column).COLUMN_NAME LIKE '%'||v_changed_counter||'%'
        THEN
          IF b_FIRST THEN
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rec_all_cols(column).column_name || ' = SELF.' || rec_all_cols(column).column_name);
          ELSE
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '       AND ' || rec_all_cols(column).column_name || ' = SELF.' || rec_all_cols(column).column_name);
          END IF;
        END IF;
      END LOOP;
    END IF;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ';' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    IF SQL%ROWCOUNT <> 1 THEN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      RAISE_APPLICATION_ERROR (-20999, ''record do not exists'');' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    END IF;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '' || chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_DELETE');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '---------------------------------------------------------------' || chr(10) || chr(10));
    
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_DELETE;

  /* ------------------------------------------------------------------------
  --Create the Select-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_SELECT(IN_TABLE_NAME  IN VARCHAR2,
                          IN_OWNER       IN VARCHAR2,
                          IN_IGNORE_LIST IN PA_ROWGENERATOR.TABLE_VARCHAR2,
                          IN_OUT_TYPE    IN OUT CLOB)
  IS    
    b_FIRST BOOLEAN;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME        = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER             = ' || IN_OWNER 
                          || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT()
                          ;
    END;
  BEGIN
    FOR IND IN rec_ind.first..rec_ind.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_SELECT(');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ')' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));

      --define Parm-Funktion
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '    FUNCTION parm RETURN CLOB IS BEGIN ' || chr(10));
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN ''PARAMETER: ''' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAM));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '         || CHR(10) || '' SELF:''  || SELF.TO_CLOB();' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' END parm;' || chr(10));
      
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELECT ');
    
      b_FIRST := TRUE;
      FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
      LOOP
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '          ,' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END LOOP;
    
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      INTO ');
    
      b_FIRST := TRUE;
      FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
      LOOP
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'SELF.' || rec_all_cols(column).column_name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '          ,SELF.' || rec_all_cols(column).column_name || chr(10));
        END IF;
      END LOOP;
    
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FROM ' || IN_OWNER || '.' || IN_TABLE_NAME || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHERE ');      
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => GET_IN_WHERE4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                              IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                              IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                              IN_COLS        => rec_all_cols)); 
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ';' || chr(10) || chr(10));
      PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_SELECT');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '---------------------------------------------------------------' || chr(10) || chr(10));
     
    END LOOP;
    
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_SELECT;

  /* ------------------------------------------------------------------------
  -- Create the Init-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_INIT(IN_TABLE_NAME   IN VARCHAR2,
                        IN_OWNER        IN VARCHAR2,
                        IN_MAX_COL_SIZE IN NUMBER,
                        IN_TABLE_ABBR   IN VARCHAR,
                        IN_OUT_TYPE     IN OUT CLOB)
  IS    
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME   = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER        = ' || IN_OWNER 
                          || CHR(10) || 'IN_MAX_COL_SIZE = ' || IN_MAX_COL_SIZE 
                          || CHR(10) || 'IN_TABLE_ABBR   = ' || IN_TABLE_ABBR;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_INIT' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
  
    FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELF.' || rpad(rec_all_cols(column).column_name, IN_MAX_COL_SIZE) || ' := NULL;' || chr(10));
    END LOOP;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_INIT');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '---------------------------------------------------------------' || chr(10) || chr(10));
    
  EXCEPTION
    WHEN OTHERS THEN     
      RAISE;
  END CREATE_INIT;

  /* ------------------------------------------------------------------------
  -- Create the Default-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_DEFAULT(IN_TABLE_NAME   IN VARCHAR2,
                           IN_OWNER        IN VARCHAR2,
                           IN_MAX_COL_SIZE IN NUMBER,
                           IN_TABLE_ABBR   IN VARCHAR2,
                           IN_OUT_TYPE     IN OUT CLOB)
  IS
    b_DEFAULT_EXISTS BOOLEAN;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_MAX_COL_SIZE  = ' || IN_MAX_COL_SIZE 
                          || CHR(10) || 'IN_TABLE_ABBR    = ' || IN_TABLE_ABBR;
    END;
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'MEMBER PROCEDURE ROW_DEFAULT' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
  
    b_DEFAULT_EXISTS := false;
    FOR defaults IN  rec_all_cols.first..rec_all_cols.last
    LOOP
      IF rec_all_cols(defaults).DATA_DEFAULT IS NOT NULL THEN
        b_DEFAULT_EXISTS := TRUE;
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE,
                        src_lob  => '    SELF.' || rpad(rec_all_cols(defaults).COLUMN_NAME, IN_MAX_COL_SIZE + 1) || ' := ' || REPLACE(rec_all_cols(defaults).DATA_DEFAULT, chr(10), '') || ';' ||
                                    chr(10));
      END IF;
    END LOOP;
    IF NOT b_DEFAULT_EXISTS THEN
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    NULL;' || chr(10));
    END IF;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
    PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_DEFAULT');
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));
    
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END CREATE_DEFAULT;

  /* ------------------------------------------------------------------------
  -- Create the Lock-Procedure

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_LOCK(IN_TABLE_NAME    IN VARCHAR2,
                        IN_OWNER         IN VARCHAR2,
                        IN_HAVE_CHANGED  IN BOOLEAN,
                        IN_TABLE_ABBR    IN VARCHAR2,
                        IN_OUT_TYPE      IN OUT CLOB)
  IS    
    b_FIRST BOOLEAN;    
    v_count NUMBER;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME    = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER         = ' || IN_OWNER 
                          || CHR(10) || 'IN_HAVE_CHANGED  = ' || CASE WHEN IN_HAVE_CHANGED THEN 'TRUE' ELSE 'FALSE' END 
                          || CHR(10) || 'IN_TABLE_ABBR    = ' || IN_TABLE_ABBR;
    END;
  BEGIN
    if IN_have_CHANGED THEN
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_LOCK' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    v_lock                 NUMBER;' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FUNCTION parm RETURN CLOB IS BEGIN ' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN ''PARAMETER: <keine>''' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '        || CHR(10) || '' SELF:'' || SELF.TO_CLOB();' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '     END;' || CHR(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));

      v_count := rec_ind.count();
      if (v_count <> 0) THEN
        IF IN_have_CHANGED THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELECT ' || v_changed_counter || chr(10));
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELECT ' || '1' || chr(10));
        END IF;
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      INTO v_lock' || chr(10));
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FROM ' || IN_OWNER || '.' || IN_TABLE_NAME || chr(10));
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHERE ');
        b_FIRST := TRUE;
        for pk in rec_primary_key.first..rec_primary_key.last
        LOOP
          IF b_FIRST THEN
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => rec_primary_key(pk).column_Name || ' = SELF.' || rec_primary_key(pk).column_Name || chr(10));
            b_FIRST := FALSE;
          ELSE
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      AND ' || rec_primary_key(pk).column_Name || ' = SELF.' || rec_primary_key(pk).column_Name || chr(10));
          END IF;
        END LOOP;
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FOR UPDATE' || chr(10));
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    ;' || chr(10));
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    IF SELF.'||v_changed_counter|| ' <> v_lock THEN' || chr(10));
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      SELF.ROW_SELECT(');
        b_FIRST := TRUE;
        for pk in rec_primary_key.first..rec_primary_key.last
        LOOP
          IF b_FIRST THEN
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    IN_' || rec_primary_key(pk).column_Name || ' => SELF.' || rec_primary_key(pk).column_Name || chr(10));
            b_FIRST := FALSE;
          ELSE
            DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '                        , IN_' || rec_primary_key(pk).column_Name || ' => SELF.' || rec_primary_key(pk).column_Name || chr(10));
          END IF;
        END LOOP;
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '                     );' || chr(10));
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    END IF;' || chr(10));
      
      ELSE
        DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    NULL;' || chr(10));
      END IF;
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
      PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE);
    END IF;
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));
  
    FOR IND IN rec_ind.first .. rec_ind.last
    LOOP
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  MEMBER PROCEDURE ROW_LOCK(');    
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAMETER));

      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ')' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
      -- define Parm function
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '    FUNCTION parm RETURN CLOB IS BEGIN ' || chr(10));
      dbms_lob.append(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN ''PARAMETER: ''' || chr(10));       
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => PA_ROWGENERATOR.GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                                                  IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                                                  IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                                                  IN_COLS        => rec_all_cols,
                                                                                                  IN_ACTION      => PA_ROWGENERATOR.C_LIST_PARAM));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '         || CHR(10) || '' SELF:''  || SELF.TO_CLOB();' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' END parm;' || chr(10));

      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));

      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    SELECT');
      b_FIRST := true;

      FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
      LOOP
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' ' || rec_all_cols(column).column_Name || chr(10));
          b_FIRST := false;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '          ,' || rec_all_cols(column).column_Name || chr(10));
        END IF;
      END LOOP;
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      INTO');
      b_FIRST := TRUE;

      FOR COLUMN IN  rec_all_cols.first..rec_all_cols.last
      LOOP
        IF b_FIRST THEN
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => ' SELF.' || rec_all_cols(column).column_Name || chr(10));
          b_FIRST := FALSE;
        ELSE
          DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '          ,' || 'SELF.' || rec_all_cols(column).column_Name || chr(10));
        END IF;
      END LOOP;
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FROM ' || IN_OWNER || '.' || IN_TABLE_NAME || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHERE ');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => GET_IN_WHERE4UINDEX(IN_TABLE_OWNER => IN_OWNER,
                                                                              IN_TABLE_NAME  => IN_TABLE_NAME,
                                                                              IN_INDEX_NAME  => rec_ind(IND).INDEX_NAME,
                                                                              IN_COLS        => rec_all_cols));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    FOR UPDATE;' || chr(10));
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10));
      PA_ROWGENERATOR.APPEND_EXCEPTION(IN_OUT_TYPE => IN_OUT_TYPE, IN_METHOD => 'ROW_LOCK');
      DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || '  ---------------------------------------------------------------' || chr(10) || chr(10));    
    END LOOP;
    
  EXCEPTION
    WHEN OTHERS THEN     
      RAISE;
  END CREATE_LOCK;

  /* ------------------------------------------------------------------------
  --  Returns values in XML Format

  --  date   : 23.10.2015
  --  author : Andreas Bachmann
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_TO_CLOB (IN_OUT_TYPE IN OUT CLOB)
  IS
  BEGIN
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => chr(10) || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'MEMBER FUNCTION TO_CLOB RETURN CLOB' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  IS' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  BEGIN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    RETURN xmltype(SELF).extract(''/'').getCLOBVal() ;' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  EXCEPTION' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '    WHEN OTHERS THEN' || chr(10));
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '      RETURN ''ERROR : ''|| SQLERRM;' || chr(10));        
    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => '  END TO_CLOB;' || chr(10));
  END CREATE_TO_CLOB;   


  /* ------------------------------------------------------------------------
  -- Create the body of the ROW-Type

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_BODY(IN_TABLE_NAME    IN VARCHAR2,
                        IN_OWNER         IN VARCHAR2,
                        IN_HAVE_CHANGED  IN BOOLEAN,
                        IN_MAX_COL_SIZE  IN NUMBER,
                        IN_TABLE_ABBR    IN VARCHAR2,
                        IN_READONLY      IN BOOLEAN,
                        IN_IGNORE_LIST   IN PA_ROWGENERATOR.TABLE_VARCHAR2,
                        IN_SEQUENCE_NAME IN VARCHAR2,
                        IN_OUT_TYPE      IN OUT CLOB)
  IS
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME        = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER             = ' || IN_OWNER                         
                          || CHR(10) || 'IN_HAVE_CHANGED      = ' || CASE WHEN IN_HAVE_CHANGED THEN 'TRUE' ELSE 'FALSE' END 
                          || CHR(10) || 'IN_MAX_COL_SIZE      = ' || TO_CHAR(IN_MAX_COL_SIZE) 
                          || CHR(10) || 'IN_TABLE_ABBR        = ' || IN_TABLE_ABBR
                          || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT
                          || CHR(10) || 'IN_SEQUENCE_NAME     = ' || IN_SEQUENCE_NAME
                          || CHR(10) || 'IN_READONLY          = ' || CASE WHEN IN_READONLY THEN 'TRUE' ELSE 'FALSE' END
                          ;
    END;
  BEGIN
    --CONSTRUCTOR
    CREATE_CONSTRUCTOR(IN_TABLE_NAME  => IN_TABLE_NAME, 
                       IN_TABLE_ABBR  => IN_TABLE_ABBR, 
                       IN_OWNER       => IN_OWNER,
                       IN_IGNORE_LIST => IN_IGNORE_LIST,
                       IN_OUT_TYPE    => IN_OUT_TYPE);
    --ROW_EXISTS  
    CREATE_EXISTS(IN_TABLE_NAME  => IN_TABLE_NAME, 
                  IN_OWNER       => IN_OWNER, 
                  IN_IGNORE_LIST => IN_IGNORE_LIST,
                  IN_OUT_TYPE    => IN_OUT_TYPE);
    -- STATIC EXIST
    CREATE_STATIC_EXIST(IN_TABLE_NAME  => IN_TABLE_NAME,
                        IN_OWNER       => IN_OWNER,
                        IN_IGNORE_LIST => IN_IGNORE_LIST,
                        IN_OUT_TYPE    => IN_OUT_TYPE );
             
    --COMPARE
    CREATE_COMPARE(IN_TABLE_NAME   => IN_TABLE_NAME,
                   IN_OWNER        => IN_OWNER,
                   IN_TABLE_ABBR   => IN_TABLE_ABBR,
                   IN_MAX_COL_SIZE => IN_MAX_COL_SIZE,
                   IN_OUT_TYPE     => IN_OUT_TYPE);
   /* readonly, create without DML operations */
   IF IN_READONLY = FALSE THEN   
      SET_PK_REC(IN_OWNER         => upper(IN_OWNER),
                 IN_TABLE_NAME    => upper(in_table_name),
                 IN_SEQUENCE_NAME => IN_SEQUENCE_NAME);                                
     --ROW_INSERT
     CREATE_INSERT(IN_TABLE_NAME    => IN_TABLE_NAME, 
                   IN_TABLE_ABBR    => IN_TABLE_ABBR, 
                   IN_OWNER         => IN_OWNER,
                   IN_SEQUENCE_NAME => IN_SEQUENCE_NAME,
                   IN_OUT_TYPE      => IN_OUT_TYPE);
     --ROW_UPDATE 
     CREATE_UPDATE(IN_TABLE_NAME    => IN_TABLE_NAME,
                   IN_OWNER         => IN_OWNER,
                   IN_HAVE_CHANGED  => IN_HAVE_CHANGED,
                   IN_MAX_COL_SIZE  => IN_MAX_COL_SIZE,
                   IN_TABLE_ABBR    => IN_TABLE_ABBR,
                   IN_SEQUENCE_NAME => IN_SEQUENCE_NAME,
                   IN_OUT_TYPE      => IN_OUT_TYPE);
     --ROW_MERGE
     CREATE_MERGE(IN_TABLE_NAME    => IN_TABLE_NAME,
                  IN_TABLE_ABBR    => IN_TABLE_ABBR,
                  IN_OWNER         => IN_OWNER,
                  IN_MAX_COL_SIZE  => IN_MAX_COL_SIZE,
                  IN_SEQUENCE_NAME => IN_SEQUENCE_NAME,
                  IN_OUT_TYPE      => IN_OUT_TYPE);
     --ROW_SAVE
     CREATE_SAVE(IN_TABLE_NAME    => IN_TABLE_NAME, 
                 IN_TABLE_ABBR    => IN_TABLE_ABBR, 
                 IN_OWNER         => IN_OWNER,
                 IN_SEQUENCE_NAME => IN_SEQUENCE_NAME,
                 IN_OUT_TYPE      => IN_OUT_TYPE);
     --ROW_DELETE    
     CREATE_DELETE(IN_TABLE_NAME    => IN_TABLE_NAME,
                   IN_OWNER         => IN_OWNER,
                   IN_HAVE_CHANGED  => IN_HAVE_CHANGED,
                   IN_MAX_COL_SIZE  => IN_MAX_COl_SIZE,
                   IN_TABLE_ABBR    => IN_TABLE_ABBR,
                   IN_SEQUENCE_NAME => IN_SEQUENCE_NAME, 
                   IN_OUT_TYPE      => IN_OUT_TYPE);
    --ROW_LOCK  
    CREATE_LOCK(IN_TABLE_NAME    => IN_TABLE_NAME,
                IN_OWNER         => IN_OWNER,
                IN_TABLE_ABBR    => IN_TABLE_ABBR,
                IN_HAVE_CHANGED  => IN_HAVE_CHANGED,
                IN_OUT_TYPE      => IN_OUT_TYPE);
    END IF;                      
    --ROW_SELECT  
    CREATE_SELECT(IN_TABLE_NAME  => IN_TABLE_NAME,
                  IN_OWNER       => IN_OWNER,
                  IN_IGNORE_LIST => IN_IGNORE_LIST,
                  IN_OUT_TYPE    => IN_OUT_TYPE );

    --ROW_INIT       
    CREATE_INIT(IN_TABLE_NAME   => IN_TABLE_NAME,
                IN_TABLE_ABBR   => IN_TABLE_ABBR,
                IN_OWNER        => IN_OWNER,
                IN_MAX_COL_SIZE => IN_MAX_COL_SIZE,
                IN_OUT_TYPE     => IN_OUT_TYPE);
    --ROW_DEFAULT
    CREATE_DEFAULT(IN_TABLE_NAME   => IN_TABLE_NAME,
                   IN_TABLE_ABBR   => IN_TABLE_ABBR,
                   IN_OWNER        => IN_OWNER,
                   IN_MAX_COL_SIZE => IN_MAX_COL_SIZE,
                   IN_OUT_TYPE     => IN_OUT_TYPE);
    -- To_CLOB
    CREATE_TO_CLOB(IN_OUT_TYPE => IN_OUT_TYPE);

    DBMS_LOB.APPEND(dest_lob => IN_OUT_TYPE, src_lob => 'END;' || chr(10));
   
  EXCEPTION
    WHEN OTHERS THEN     
      RAISE;
  END CREATE_BODY;

  /* ------------------------------------------------------------------------
  -- Compute a hash value for a table name

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  FUNCTION CREATE_HASHVALUE(IN_NAME IN VARCHAR2) RETURN CHAR 
  IS    
    v_hashvalue DECIMAL(11) := 0;
    result      VARCHAR(30);
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_NAME¿= ' || IN_NAME;
    END;
  BEGIN
    -- hash formula=|ch[0]*31^(n-1)+ch[1]*31^(n-2)+...+ch[n-1]|
    FOR idx IN 1 .. length(IN_NAME)
    LOOP
      v_hashvalue := mod(v_hashvalue, 4294967296) + mod(ASCII(substr(upper(IN_NAME), idx, 1)) * power(31, length(IN_NAME) - idx), 4294967296);
    END LOOP;
    v_hashvalue := mod(v_hashvalue, 4294967296);
    IF v_hashvalue > 2147483647 THEN
      v_hashvalue := v_hashvalue - 2147483648;
    ENd IF;
    v_hashvalue := abs(v_hashvalue);
    result    := TRIM(TO_CHAR(abs(v_hashvalue), '999999999999999999999999999999'));
    return result;
  EXCEPTION
    WHEN OTHERS THEN     
      RAISE;
  END CREATE_HASHVALUE;

  /* ------------------------------------------------------------------------
  -- Create the TAB_ROW

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  PROCEDURE CREATE_TAB_ROW(IN_TABLE_NAME   IN VARCHAR2,
                           IN_OWNER        IN VARCHAR2,
                           IN_ROW_ABK      IN VARCHAR2,
                           IN_TAB_ROW_ABBR IN VARCHAR2,
                           IN_OUT_TYPE     IN OUT CLOB)
  IS
    v_hashvalue       VARCHAR2(30);
    v_tab_row         VARCHAR2(30);
    TOO_LONG_TAB_ABBR EXCEPTION;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME   = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER        = ' || IN_OWNER 
                          || CHR(10) || 'IN_TAB_ROW_ABBR = ' || IN_TAB_ROW_ABBR;
    END;
  BEGIN 
    IF length(IN_TABLE_NAME) > 22 OR IN_TAB_ROW_ABBR IS NOT NULL  THEN
      IF IN_TAB_ROW_ABBR IS NULL THEN
          v_hashvalue := CREATE_hashvalue(IN_TABLE_NAME);
          v_tab_ROW   := substr(IN_TABLE_NAME, 1, 22 - length(v_hashvalue)) || v_hashvalue;
      ELSE
        IF length(IN_TAB_ROW_ABBR) <= 26 THEN
          v_tab_ROW := IN_TAB_ROW_ABBR;
        ELSE
          RAISE TOO_LONG_TAB_ABBR;
        END IF;
      END IF;
    ELSE
      v_tab_row := IN_TABLE_NAME;  
    END IF;
    dbms_lob.append(dest_lob => IN_OUT_TYPE, 
                    src_lob => 'CREATE OR REPLACE TYPE ' ||  IN_OWNER || '.TAB_ROW_' || v_tab_row || ' FORCE AS TABLE OF ' || in_owner || '.ROW_' || IN_ROW_ABK || ';');
       
    EXCEPTION
      WHEN TOO_LONG_TAB_ABBR THEN      
       RAISE_APPLICATION_ERROR(-20000, 'Table abbreviation is too long.');
      WHEN OTHERS THEN NULL; 
  END CREATE_TAB_ROW;

  /* ------------------------------------------------------------------------
  -- Check if a table exists

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  FUNCTION TABLE_EXISTS(IN_TABLE_NAME IN VARCHAR2,
                        IN_OWNER      IN VARCHAR2) RETURN BOOLEAN 
  IS    
    v_count NUMBER := 0;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER      = ' || IN_OWNER;
    END;
  BEGIN
  
    SELECT count(*)
    INTO v_count
    FROM ALL_ALL_TABLES ATT
    WHERE ATT.OWNER      = IN_OWNER
      AND ATT.table_name = IN_TABLE_NAME;
    
    RETURN(v_count <> 0);
  
  EXCEPTION
    WHEN OTHERS THEN     
      RAISE;   
  END TABLE_EXISTS;



  /* ------------------------------------------------------------------------
  -- Build the complete Row-type

  -- date   : 12.08.2014
  -- author : Thomas Maier
  ------------------------------------------------------------------------ */
  FUNCTION CREATE_ROWTYPE(IN_TABLE_NAME    IN VARCHAR2,
                          IN_OWNER         IN VARCHAR2,  
                          IN_ROW_ABBR      IN VARCHAR2 := NULL,
                          IN_TAB_ROW_ABBR  IN VARCHAR2 := NULL,
                          IN_WITH_TAB_ROW  IN BOOLEAN  := FALSE,
                          IN_READONLY      IN BOOLEAN  := FALSE,
                          IN_IGNORE_LIST   IN PA_ROWGENERATOR.TABLE_VARCHAR2 := NULL,
                          IN_SEQUENCE_NAME IN VARCHAR2 := NULL) RETURN CLOB 
  IS    
    v_count                NUMBER := 0;
    b_have_changed         BOOLEAN;
    v_hashvalue            VARCHAR(30);
    v_table_name           VARCHAR(26);
    v_maxcolsize           NUMBER;
    v_sequence_name        VARCHAR2(50);
    v_type_spec            CLOB;
    v_type_body            CLOB;
    v_tab_row              CLOB;
    v_complete             CLOB;
    TABLE_DOESNT_EXIST     EXCEPTION;
    TOO_LONG_ABBR          EXCEPTION;
    SEQUENCE_NOT_AVAILABLE EXCEPTION;
    
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' || CHR(10) || 'IN_TABLE_NAME        = ' || IN_TABLE_NAME 
                          || CHR(10) || 'IN_OWNER             = ' || IN_OWNER
                          || CHR(10) || 'IN_ROW_ABBR          = ' || IN_ROW_ABBR
                          || CHR(10) || 'IN_TAB_ROW_ABBR      = ' || IN_TAB_ROW_ABBR
                          || CHR(10) || 'IN_WITH_TAB_ROW      = ' || CASE WHEN IN_WITH_TAB_ROW THEN 'TRUE' ELSE 'FALSE' END
                          || CHR(10) || 'IN_READONLY          = ' || CASE WHEN IN_READONLY THEN 'TRUE' ELSE 'FALSE' END
                          || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT
                          || CHR(10) || 'IN_SEQUENCE_NAME     = ' || IN_SEQUENCE_NAME;
    END;
  BEGIN
    IF TABLE_EXISTS(IN_TABLE_NAME => upper(IN_TABLE_NAME), IN_OWNER => upper(IN_OWNER))
    THEN
      SELECT count(*)
      INTO v_count
      FROM ALL_TAB_COLS ATC
      WHERE ATC.HIDDEN_COLUMN = 'NO'
        AND ATC.OWNER         = IN_OWNER
        AND ATC.TABLE_NAME    = IN_TABLE_NAME
        AND ATC.COLUMN_NAME LIKE '%'||v_changed_counter||'%';
        
      IF (v_count <> 0) 
      THEN
        b_have_changed := true;
      END IF;
    
      SELECT max(length(COLUMN_NAME))
      INTO v_maxcolsize
      FROM ALL_TAB_COLS ATC
      WHERE ATC.HIDDEN_COLUMN = 'NO'
        AND ATC.OWNER         = IN_OWNER
        AND ATC.TABLE_NAME    = IN_TABLE_NAME;
    
      IF length(IN_TABLE_NAME) > 26 OR IN_ROW_ABBR IS NOT NULL 
      THEN
        IF IN_ROW_ABBR IS NULL 
        THEN
          v_hashvalue  := CREATE_hashvalue(IN_TABLE_NAME);
          v_table_name := substr(IN_TABLE_NAME, 1, 26 - length(v_hashvalue)) || v_hashvalue;
        ELSE
          IF length(IN_ROW_ABBR) <= 26 
          THEN
            v_table_name := IN_ROW_ABBR;
          ELSE
            RAISE TOO_LONG_ABBR;
          END IF;
        END IF;
      ELSE
        v_table_name := IN_TABLE_NAME;
      END IF;
      
      -- test if sequence is available      
      IF IN_READONLY = FALSE THEN
         IF IN_SEQUENCE_NAME IS NULL THEN
           FOR rec in ( SELECT als.SEQUENCE_NAME
                          FROM ALL_CONSTRAINTS cons
                          JOIN ALL_CONS_COLUMNS cols on ( cons.owner = cols.owner AND cons.table_name = cols.table_name and cons.constraint_name = cols.constraint_name)
                          JOIN ALL_SEQUENCES als on ( als.SEQUENCE_OWNER = cons.owner AND als.SEQUENCE_NAME = v_sequence_prefix || cons.table_name || v_sequence_suffix)
                         WHERE cons.owner        = IN_OWNER
                           AND cons.table_name   = IN_TABLE_NAME
                           AND cons.constraint_type = 'P')
           LOOP
              v_sequence_name := rec.Sequence_Name;
           END LOOP;            
         ELSE
           FOR rec IN ( SELECT als.SEQUENCE_NAME
                          FROM ALL_SEQUENCES als
                         WHERE als.SEQUENCE_OWNER = IN_OWNER
                           AND als.SEQUENCE_NAME = IN_SEQUENCE_NAME)
           LOOP            
            v_sequence_name := rec.Sequence_Name;
           END LOOP; 
         END IF;
         
         IF v_sequence_name is null THEN
           RAISE SEQUENCE_NOT_AVAILABLE;
         END IF;  
      END IF;

      DBMS_LOB.createtemporary(v_TYPE_SPEC, true);
      DBMS_LOB.createtemporary(v_TYPE_BODY, true);

      -- collecting needed information
      SET_COL_REC(IN_OWNER      => upper(IN_OWNER),
                  IN_TABLE_NAME => upper(in_table_name));
      SET_IND_REC(IN_OWNER       => upper(IN_OWNER),
                  IN_TABLE_NAME  => upper(in_table_name),
                  IN_IGNORE_LIST => IN_IGNORE_LIST);

      CREATE_SPEC(IN_TABLE_NAME   => upper(in_table_name),
                  IN_TABLE_ABBR   => upper(v_table_name),
                  IN_OWNER        => upper(IN_OWNER),
                  IN_HAVE_CHANGED => b_have_changed,
                  IN_MAX_COL_SIZE => v_maxcolsize,
                  IN_TAB_ROW_ABBR => IN_TAB_ROW_ABBR,
                  IN_WITH_TAB_ROW => IN_WITH_TAB_ROW,
                  IN_READONLY     => IN_READONLY,
                  IN_IGNORE_LIST  => IN_IGNORE_LIST,
                  IN_OUT_TYPE     => v_TYPE_SPEC);

      CREATE_BODY(IN_TABLE_NAME    => upper(in_table_name),
                  IN_TABLE_ABBR    => upper(v_table_name),
                  IN_OWNER         => upper(IN_OWNER),
                  IN_HAVE_CHANGED  => b_have_changed,
                  IN_MAX_COL_SIZE  => v_maxcolsize,
                  IN_READONLY      => IN_READONLY,
                  IN_IGNORE_LIST   => IN_IGNORE_LIST,
                  IN_SEQUENCE_NAME => v_sequence_name,
                  IN_OUT_TYPE      => v_TYPE_BODY);
       
                 
      IF IN_WITH_TAB_ROW THEN
        DBMS_LOB.createtemporary(v_TAB_ROW, true);
        CREATE_TAB_ROW(IN_TABLE_NAME      => upper(in_table_name),
                       IN_OWNER           => upper(in_owner),
                       IN_ROW_ABK         => upper(v_table_name),
                       IN_TAB_ROW_ABBR    => upper(IN_TAB_ROW_ABBR),
                       IN_OUT_TYPE        => v_TAB_ROW);
      END IF;
     
      dbms_lob.createtemporary(v_complete,true);
      dbms_lob.append(dest_lob => v_complete, src_lob =>v_TYPE_SPEC);
      dbms_lob.append(dest_lob => v_complete, src_lob =>'/' || chr(10));
      dbms_lob.append(dest_lob => v_complete, src_lob =>v_TYPE_BODY);
      dbms_lob.append(dest_lob => v_complete, src_lob =>'/' || chr(10));
      IF IN_WITH_TAB_ROW THEN
        dbms_lob.append(dest_lob => v_complete, src_lob =>v_TAB_ROW);
        dbms_lob.append(dest_lob => v_complete, src_lob =>chr(10) || '/' || chr(10));        
        DBMS_LOB.freetemporary(v_TAB_ROW);
      END IF;
      IF IN_READONLY = FALSE THEN
        CREATE_INSERT_TRIGGER(IN_TABLE_NAME    => UPPER (IN_TABLE_NAME),
                              IN_OWNER         => UPPER (IN_OWNER),
                              IN_SEQUENCE_NAME => UPPER (v_sequence_name),
                              IN_OUT_TYPE      => v_complete);
                              
        CREATE_UPDATE_TRIGGER(IN_TABLE_NAME    => UPPER (IN_TABLE_NAME),
                              IN_OWNER         => UPPER (IN_OWNER),
                              IN_OUT_TYPE      => v_complete);
      END IF;
      DBMS_LOB.freetemporary(v_TYPE_SPEC);
      DBMS_LOB.freetemporary(v_TYPE_BODY);

      RETURN v_complete;
    ELSE
      RAISE TABLE_DOESNT_EXIST;
    END IF;
  EXCEPTION
    WHEN TOO_LONG_ABBR THEN
      RAISE_APPLICATION_ERROR(-20000, 'IN_ROW_ABBR has more than 26 chars');
    WHEN TABLE_DOESNT_EXIST THEN
      RAISE_APPLICATION_ERROR(-20000,'Table doesn''t exist');
    WHEN SEQUENCE_NOT_AVAILABLE THEN
      RAISE_APPLICATION_ERROR (-20000, 'Sequence not found');  
    WHEN OTHERS THEN
      RAISE;
  END CREATE_ROWTYPE;

  /* ------------------------------------------------------------------------
  -- returns IN_PARAMETER list for methods based on a UNIQUE INDEX
       
  -- date   : 18.04.2015
  -- author : Andreas Bachmann 
  ------------------------------------------------------------------------ */
  FUNCTION GET_IN_PARAMETER4UINDEX(IN_TABLE_OWNER IN VARCHAR2,
                                   IN_TABLE_NAME  IN VARCHAR2,
                                   IN_INDEX_NAME  IN VARCHAR2,
                                   IN_COLS        IN OUT NOCOPY PA_ROWGENERATOR.COL_REC_TAB,
                                   IN_ACTION      IN VARCHAR2) RETURN VARCHAR2
  IS
    v_result VARCHAR2(32000);
    tab_rec  PA_ROWGENERATOR.COL_REC_TAB;
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:'
             || CHR(10) || 'IN_TABLE_OWNER = ' || IN_TABLE_OWNER
             || CHR(10) || 'IN_TABLE_NAME  = ' || IN_TABLE_NAME
             || CHR(10) || 'IN_INDEX_NAME  = ' || IN_INDEX_NAME
             || CHR(10) || 'IN_COLS.COUNT  = ' || IN_COLS.COUNT()
             || CHR(10) || 'IN_ACTION      = ' || IN_ACTION;
    END;
  BEGIN
    IF NOT b_init_loaded_parameter4uindex
    THEN
      PA_ROWGENERATOR.SET_U_IND_REC(IN_TABLE_OWNER => IN_TABLE_OWNER,
                                    IN_TABLE_NAME  => IN_TABLE_NAME,
                                    IN_COLS        => IN_COLS);
    END IF;
    
    tab_rec := new PA_ROWGENERATOR.COL_REC_TAB();
     
    FOR rec in rec_u_ind.first .. rec_u_ind.last
    LOOP      
      IF rec_u_ind(rec).INDEX_NAME = IN_INDEX_NAME
      THEN
        /* many columns in one index expression possible */
        FOR i IN 1..IN_COLS.COUNT LOOP
          IF INSTR (STR1 => rec_u_ind(rec).column_name_p4u , STR2 => '"'|| IN_COLS(i).COLUMN_NAME ||'"') > 0 THEN              
            tab_rec  := add_distinct_rec ( IN_LIST =>  tab_rec, IN_REC => IN_COLS(i));                        
          END IF;
        END LOOP;         
      END IF;
    END LOOP;                   

    FOR r in 1 ..tab_rec.count LOOP                 
      IF r = 1 THEN
        CASE 
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_PARAMETER  AND tab_rec(r).COLUMN_NAME = 'GELOESCHT_ZST' THEN
            v_result := 'IN_' || tab_rec(r).COLUMN_NAME || ' IN ' || tab_rec(r).DATA_TYPE || ' DEFAULT NULL ';
          WHEN IN_ACTION =  PA_ROWGENERATOR.C_LIST_PARAMETER THEN
            v_result := 'IN_' || tab_rec(r).COLUMN_NAME || ' IN ' || tab_rec(r).DATA_TYPE;
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_METHODENAME THEN
            v_result := 'IN_' || tab_rec(r).COLUMN_NAME || ' ' || tab_rec(r).DATA_TYPE;
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_CALL_METHOD THEN     
            v_result := 'IN_' ||tab_rec(r).COLUMN_NAME || ' => ' || 'IN_' || tab_rec(r).COLUMN_NAME ;
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_PARAM THEN
            v_result :=  '        || CHR(10) || ''' || ' IN_' || tab_rec(r).COLUMN_NAME || ' = '' ||' || ' IN_' || tab_rec(r).COLUMN_NAME;
          ELSE
               NULL;
        END CASE;   
      ELSE
        CASE 
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_PARAMETER  AND tab_rec(r).COLUMN_NAME = 'GELOESCHT_ZST' THEN
            v_result := v_result || ', IN_' || tab_rec(r).COLUMN_NAME || ' IN ' || tab_rec(r).DATA_TYPE || ' DEFAULT NULL ';                      
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_PARAMETER THEN
            v_result := v_result || ', IN_' || tab_rec(r).COLUMN_NAME || ' IN ' || tab_rec(r).DATA_TYPE;         
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_METHODENAME THEN
            v_result := v_result || ', IN_' || tab_rec(r).COLUMN_NAME || ' ' || tab_rec(r).DATA_TYPE;
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_CALL_METHOD THEN     
            v_result := v_result || CHR(10) || '                , IN_' ||  tab_rec(r).COLUMN_NAME || ' => IN_' || tab_rec(r).COLUMN_NAME ;
          WHEN IN_ACTION = PA_ROWGENERATOR.C_LIST_PARAM THEN                      
            v_result := v_result || CHR(10) || '        || CHR(10) || ''' || ' IN_' || tab_rec(r).COLUMN_NAME || ' = '' ||' || ' IN_' || tab_rec(r).COLUMN_NAME;
          ELSE
            NULL;
        END CASE;       
      END IF;
    END LOOP;  

    return v_result;
 EXCEPTION 
    WHEN OTHERS THEN 
      RAISE;
 END GET_IN_PARAMETER4UINDEX; 
 
  
  /* ------------------------------------------------------------------------
  -- returns where-list for methods based on a UNIQUE INDEX

  -- date   : 08.05.2015
  -- author : Andreas Bachmann
  ------------------------------------------------------------------------ */
  FUNCTION GET_IN_WHERE4UINDEX(IN_TABLE_OWNER IN VARCHAR2,
                               IN_TABLE_NAME  IN VARCHAR2,
                               IN_INDEX_NAME  IN VARCHAR2,
                               IN_COLS IN OUT NOCOPY PA_ROWGENERATOR.COL_REC_TAB) RETURN VARCHAR2
  IS    
    v_result   VARCHAR2(4000); 
    tab_column PA_ROWGENERATOR.TABLE_VARCHAR2;
    tab_null   PA_ROWGENERATOR.TABLE_VARCHAR2;
    b_jump     BOOLEAN;

    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' 
             || CHR(10) || 'IN_TABLE_OWNER = ' || IN_TABLE_OWNER
             || CHR(10) || 'IN_TABLE_NAME  = ' || IN_TABLE_NAME
             || CHR(10) || 'IN_INDEX_NAME  = ' || IN_INDEX_NAME
             || CHR(10) || 'IN_COLS.COUNT  = ' || IN_COLS.COUNT();
    END;
  BEGIN 
    
    IF NOT b_init_loaded_parameter4uindex
    THEN
      PA_ROWGENERATOR.SET_U_IND_REC(IN_TABLE_OWNER => IN_TABLE_OWNER,
                                    IN_TABLE_NAME  => IN_TABLE_NAME,
                                    IN_COLS        => IN_COLS);
    END IF;
    
    tab_column := new PA_ROWGENERATOR.TABLE_VARCHAR2();
    tab_null   := new PA_ROWGENERATOR.TABLE_VARCHAR2();

    FOR rec in rec_u_ind.first .. rec_u_ind.last
    LOOP
      IF rec_u_ind(rec).INDEX_NAME = IN_INDEX_NAME
      THEN
        b_jump := true;
        
        tab_column.extend();
        tab_null.extend();           
        tab_column(tab_column.count()) := rec_u_ind(rec).column_name_w4u;
        tab_null( tab_null.count()) := 'N'; 
        /* many columns in one index expression possible */
        FOR i IN 1..IN_COLS.COUNT 
        LOOP
          IF INSTR (STR1 => rec_u_ind(rec).column_name_w4u , STR2 => '"'|| IN_COLS(i).COLUMN_NAME ||'"') > 0 
          THEN
            IF IN_COLS(i).nullable = 'Y' 
            THEN
              tab_null(tab_null.count) := 'Y';
            END IF;   
              tab_column(tab_column.count()) := replace ( tab_column(tab_column.count()), '"'|| IN_COLS(i).COLUMN_NAME ||'"' , ' IN_' || IN_COLS(i).COLUMN_NAME ||' ' );
          END IF;
        END LOOP;         
        
        IF tab_column.count() = 1 
        THEN
          IF tab_null(tab_null.count()) = 'Y' 
          THEN
            v_result := '          ( ' || rec_u_ind(rec).column_name_w4u || ' = ' || tab_column ( tab_column.count()) || ' OR ( ' || rec_u_ind(rec).column_name_w4u || ' IS NULL AND ' || tab_column ( tab_column.count()) || ' IS NULL ))';
          ELSE
            v_result := rec_u_ind(rec).column_name_w4u || ' = ' || tab_column ( tab_column.count());
          END IF;
        ELSE
          IF  tab_null(tab_null.count()) = 'Y' 
          THEN
            v_result := v_result || chr(10) || '         AND ( ' || rec_u_ind(rec).column_name_w4u || ' = ' || tab_column ( tab_column.count()) || ' OR ( ' || rec_u_ind(rec).column_name_w4u || ' IS NULL AND ' || tab_column ( tab_column.count()) || ' IS NULL ))';      
          ELSE
            v_result := v_result || chr(10) || '          AND ' ||  rec_u_ind(rec).column_name_w4u || ' = ' || tab_column ( tab_column.count());
          END IF;
        END IF;         
      END IF;
    END LOOP; 

    RETURN v_result;
  EXCEPTION 
     WHEN OTHERS THEN         
      RAISE;
  END GET_IN_WHERE4UINDEX;                                

  /* ------------------------------------------------------------------------
  -- emulates "multiset union distinct" for given record

  -- date   : 07.05.2015
  -- author : Andreas Bachmann
  ------------------------------------------------------------------------ */
  FUNCTION ADD_DISTINCT_REC(IN_LIST IN PA_ROWGENERATOR.COL_REC_TAB , 
                            IN_REC  IN PA_ROWGENERATOR.COL_REC) RETURN PA_ROWGENERATOR.COL_REC_TAB
  IS      
    tab1    PA_ROWGENERATOR.COL_REC_TAB;  
    b_found BOOLEAN := FALSE;
   
   FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter:' 
             || CHR(10) || 'IN_LIST.COUNT      = ' || IN_LIST.COUNT
             || CHR(10) || 'IN_REC.COLUMN_NAME = ' || IN_REC.COLUMN_NAME
             ;
    END;
  BEGIN
    IF    IN_LIST IS NULL 
       OR IN_LIST.count = 0 
    THEN
      tab1 := new PA_ROWGENERATOR.COL_REC_TAB ();      
      tab1.extend();
      tab1(1) := IN_REC;
    ELSE
      tab1 := IN_LIST;
      FOR i IN 1 .. tab1.count() 
      LOOP
         IF tab1(i).COLUMN_NAME = IN_REC.COLUMN_NAME 
         THEN
            b_found := TRUE;
         END IF;   
      END LOOP;
      
      IF b_found = FALSE 
      THEN
         tab1.extend();
         tab1(tab1.count()) := IN_REC;
      END IF;
    END IF;
   
    RETURN tab1;
   
  EXCEPTION 
    WHEN OTHERS THEN         
      RAISE;   
  END ADD_DISTINCT_REC; 

  /* ------------------------------------------------------------------------
  -- write generator parameters into generated RowType
     
  -- date   : 08.06.2015
  -- author : Andreas Bachmann
  ------------------------------------------------------------------------ */
  FUNCTION ADD_GENERATOR_PARAMETER_DOC(IN_TABLE_NAME   IN VARCHAR2,
                                       IN_OWNER        IN VARCHAR2,                                       
                                       IN_ROW_ABBR     IN VARCHAR2,
                                       IN_TAB_ROW_ABBR IN VARCHAR2,
                                       IN_WITH_TAB_ROW IN BOOLEAN,
                                       IN_READONLY     IN BOOLEAN,
                                       IN_IGNORE_LIST  IN PA_ROWGENERATOR.TABLE_VARCHAR2) RETURN VARCHAR2 
  IS   
    v_text         VARCHAR2(4000);
    v_generator_db VARCHAR2(100);
    FUNCTION parm RETURN VARCHAR2 IS
    BEGIN
      RETURN 'Parameter: ' || CHR(10) || 'IN_TABLE_NAME        = ' || IN_TABLE_NAME 
                           || CHR(10) || 'IN_OWNER             = ' || IN_OWNER 
                           || CHR(10) || 'IN_ROW_ABBR          = ' || IN_ROW_ABBR
                           || CHR(10) || 'IN_TAB_ROW_ABBR      = ' || IN_TAB_ROW_ABBR
                           || CHR(10) || 'IN_WITH_TAB_ROW      = ' || CASE WHEN IN_WITH_TAB_ROW THEN 'TRUE' ELSE 'FALSE' END
                           || CHR(10) || 'IN_READONLY          = ' || CASE WHEN IN_READONLY THEN 'TRUE' ELSE 'FALSE' END                      
                           || CHR(10) || 'IN_IGNORE_LIST.COUNT = ' || IN_IGNORE_LIST.COUNT
                           ;
    END;
  BEGIN
    SELECT sys_context ('USERENV','DB_NAME') 
      INTO v_generator_db
      FROM dual;
   
    v_text :=            CHR(10) || ' /*  ---------------------------------------------------- ';
    v_text := v_text  || CHR(10) || '     Generator@'|| v_generator_db     ||'     ' || PA_ROWGENERATOR.C_VERSION ;
    v_text := v_text  || CHR(10) || '     Parameter                                            ';
    v_text := v_text  || CHR(10) || '     ---------------------------------------------------- ';
    v_text := v_text  || CHR(10) || '     IN_TABLE_NAME                  = ' || IN_TABLE_NAME;
    v_text := v_text  || CHR(10) || '     IN_OWNER                       = ' || IN_OWNER;
    v_text := v_text  || CHR(10) || '     IN_ROW_ABBR                    = ' || IN_ROW_ABBR;
    v_text := v_text  || CHR(10) || '     IN_TAB_ROW_ABBR                = ' || IN_TAB_ROW_ABBR;
    v_text := v_text  || CHR(10) || '     IN_WITH_TAB_ROW                = ' || CASE WHEN IN_WITH_TAB_ROW IS NULL THEN 'NULL'
                                                                                     WHEN IN_WITH_TAB_ROW = TRUE  THEN 'TRUE'
                                                                                     ELSE 'FALSE'
                                                                                END;
    v_text := v_text  || CHR(10) || '     IN_READONLY                    = ' || CASE WHEN IN_READONLY IS NULL THEN 'NULL'
                                                                                     WHEN IN_READONLY = TRUE THEN 'TRUE'
                                                                                     ELSE 'FALSE'
                                                                                END;
    IF IN_IGNORE_LIST IS NULL OR IN_IGNORE_LIST.COUNT = 0 
    THEN
      v_text := v_text  || CHR(10) || '     IN_IGNORE_LIST                 = ' || 'NULL';
    ELSE 
      FOR rec in 1 .. IN_IGNORE_LIST.COUNT() 
      LOOP
        IF rec = 1
        THEN
           v_text := v_text  || CHR(10) || '     IN_IGNORE_LIST                 = ' || IN_IGNORE_LIST(rec);
        ELSE
           v_text := v_text  || CHR(10) || '                                    , ' || IN_IGNORE_LIST(rec);    
        END IF;
      END LOOP;
    END IF;
   v_text := v_text  || CHR(10) || ' */ ' || CHR(10);     

  return v_text; 
  EXCEPTION 
    WHEN OTHERS THEN       
      RAISE;     
  END  ADD_GENERATOR_PARAMETER_DOC;

  /* ------------------------------------------------------------------------
  -- load Column expression
     
  -- date   : 07.05.2015
  -- author : Andreas Bachmann
  ------------------------------------------------------------------------ */
  FUNCTION GET_EXPRESSION(IN_TABLE_NAME      IN VARCHAR2,
                          IN_TABLE_OWNER     IN VARCHAR2,
                          IN_INDEX_NAME      IN VARCHAR2,
                          IN_COLUMN_POSITION IN NUMBER) RETURN VARCHAR2
  IS
    v_result VARCHAR2(32767);
  BEGIN
    SELECT AIE.COLUMN_EXPRESSION
      INTO v_result
      FROM ALL_IND_EXPRESSIONS AIE
     WHERE AIE.INDEX_NAME = IN_INDEX_NAME
       AND AIE.TABLE_OWNER = IN_TABLE_OWNER
       AND AIE.TABLE_NAME  = IN_TABLE_NAME
       AND AIE.COLUMN_POSITION = IN_COLUMN_POSITION;

    RETURN v_result;
  END GET_EXPRESSION;
END PA_ROWGENERATOR;
/
