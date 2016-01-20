CREATE OR REPLACE PACKAGE PA_ROWGENERATOR IS
  /* ------------------------------------------------------------------------
  -- This is the new RowType-Generator
  -- a generator for pl/sql object types which represent table data

  -- Copyright (C) 2016 Joh. Berenberg, Gossler & Co. KG
  --                    Jungfernstieg 20
  --                    D - 20354 Hamburg
  --                    Contact : open_source@berenberg.de

  -- This program is free software: you can redistribute it and/or modify
  -- it under the terms of the GNU General Public License as published by
  -- the Free Software Foundation, either version 2 of the License, or
  -- (at your option) any later version.

  -- This program is distributed in the hope that it will be useful,
  -- but WITHOUT ANY WARRANTY; without even the implied warranty of
  -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  -- GNU General Public License for more details.

  -- You should have received a copy of the GNU General Public License
  -- along with this program.  If not, see <http://www.gnu.org/licenses/>.
  ------------------------------------------------------------------------ */

  /* ------------------------------------------------------------------------
  -- Type to be used by users in CREATE_ROWTYPE
  ------------------------------------------------------------------------ */
  TYPE TABLE_VARCHAR2 IS TABLE OF VARCHAR2(4000);

  /* ------------------------------------------------------------------------
  -- package attributes
  ------------------------------------------------------------------------ */
  -- constants for sequence that fills a tables' primary key (if standardized)
  v_sequence_prefix  CONSTANT VARCHAR2(20) := '';                -- sequence prefix
  v_sequence_suffix  CONSTANT VARCHAR2(20) := '_SEQ';            -- sequence suffix 

  -- constants for standard columm names
  v_created_user     CONSTANT VARCHAR2(20) := 'CREATED_USER';    -- user who created the record
  v_created_date     CONSTANT VARCHAR2(20) := 'CREATED_DATE';    -- date the record was created
  v_changed_user     CONSTANT VARCHAR2(20) := 'CHANGED_USER';    -- user who made last change on record
  v_changed_date     CONSTANT VARCHAR2(20) := 'CHANGED_DATE';    -- date the record was last changed
  v_changed_counter  CONSTANT VARCHAR2(20) := 'CHANGED_COUNTER'; -- times the record has been changed
  v_deleted_date     CONSTANT VARCHAR2(20) := 'DELETED_DATE';    -- date of (logical) delete of record
  
  -- set of standard columns
  t_std_cols         PA_ROWGENERATOR.TABLE_VARCHAR2 := PA_ROWGENERATOR.TABLE_VARCHAR2(v_created_user,
                                                                                      v_created_date,
                                                                                      v_changed_user, 
                                                                                      v_changed_date,
                                                                                      v_changed_counter,
                                                                                      v_deleted_date);

  /* ------------------------------------------------------------------------
  -- Name       : CREATE_ROWTYPE

  -- Purpose    : function to generate a Rowtype

  -- Parameters : IN_TABLE_NAME    -- name  of underlying table for rowtype
                  IN_OWNER         -- owner     - | | -         - | | -
                  IN_ROW_ABBR      -- (optional) custom abbreviation for rowtype (Default: ROW_<<TABLE_NAME>>)
                  IN_TAB_ROW_ABBR  -- (optional) custom abbreviation for table of rowtype, max. 26 characters (Default: TAB_ROW_<<TABLE_NAME>>)
                  IN_WITH_TAB_ROW  -- (optional) flag to show if Type as table of Rowtype should be created (Default: false)
                  IN_READONLY      -- (optional) flag to show if manipulation functions (insert/update/delete/save/merge/lock) should be left out (Default: false)
                  IN_IGNORE_LIST   -- (optional) list of Indexes that should not be used by the generator
                  IN_SEQUENCE_NAME -- (optional) name of sequence that fills the tables' primary key (Default: <<v_sequence_suffix>> || <<TABLE_NAME>> || <<v_sequence_suffix>>)

  -- Returns    : RowType as CLOB
  ------------------------------------------------------------------------ */
  FUNCTION CREATE_ROWTYPE(IN_TABLE_NAME     IN VARCHAR2,
                          IN_OWNER          IN VARCHAR2,
                          IN_ROW_ABBR       IN VARCHAR2 := NULL,
                          IN_TAB_ROW_ABBR   IN VARCHAR2 := NULL,
                          IN_WITH_TAB_ROW   IN BOOLEAN  := FALSE,
                          IN_READONLY       IN BOOLEAN  := FALSE,
                          IN_IGNORE_LIST    IN PA_ROWGENERATOR.TABLE_VARCHAR2 := NULL,
                          IN_SEQUENCE_NAME  IN VARCHAR2 := NULL) RETURN CLOB; 


  /* ------------------------------------------------------------------------
  -- Function for internal use only
  ------------------------------------------------------------------------ */
  FUNCTION GET_EXPRESSION(IN_TABLE_NAME      IN VARCHAR2,
                          IN_TABLE_OWNER     IN VARCHAR2,
                          IN_INDEX_NAME      IN VARCHAR2,
                          IN_COLUMN_POSITION IN NUMBER) RETURN VARCHAR2;
  
END;
/
