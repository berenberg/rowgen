
# PL/SQL rowtype generator
 
 
## <a name="whatisthis"></a> 1. What is this?
 
This generator creates source code for creating an Oracle Object Type (for further information see https://docs.oracle.com/cd/B10501_01/appdev.920/a96624/10_objs.htm) based on a tables’ metadata that provide DDL/DML functionality. Type inheritance is possible, too, so that subtypes can use the supertypes’ functions.
 
 
## <a name="prerequisites"></a> 2. Prerequisites
 
- the generator's owner has to have the permission to see the table (for which the type shall be generated) and the data dictionary
- there has to be a numeric primary key on the table (index + constraint)
- the generator uses unique indexes on a table to create the type's constructors, so at least one is needed
- to support DML operations a sequence to generate the primary key is needed. By default, the sequence's name has to be <<TABLE_NAME>>_SEQ, but you can modify the generators' package specification (v_sequence_prefix  and v_sequence_suffix) to match your personal needs
- to lock rows, you need (per default) the following columns in your table (like the sequence pre- and suffix, also the column names can be modified in the package specification):
   - CREATED_USER    VARCHAR2(100)
   - CREATED_DATE    DATE
   - CHANGED_USER    VARCHAR2(100)
   - CHANGED_DATE    DATE
   - CHANGED_COUNTER NUMBER(12)
   - DELETED_DATE    DATE
 
 
## <a name="installation"></a> 3. Installation
 
1. Download content from "src"-folder
2. install package specification and body into your database
 
 
## <a name="additionalinformation"></a> 4. Additional information
 
- this generator works with function-based indexes as unique indexes
- the generated types are good for single record processing. Sometimes direct inserts are necessary (e.g. for batch processing) because of performance reasons. To use both modes we recommendinstalling the generated triggers into your database.
 
 
## <a name="license"></a> 5. License
 
Copyright (C) 2016 Joh. Berenberg, Gossler & Co. KG. All rights reserved.
 
You may not use the identified files except in compliance with the Apache License, Version 2.0 (the "License.")
 
You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.
 
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 
See the License for the specific language governing permissions and limitations under the License.
