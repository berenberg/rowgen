
# PL/SQL rowtype generator
 
 
## <a name="whatisthis"></a> 1. What is this?
 
This generator creates source code for creating an Oracle Object Type (for further information see https://docs.oracle.com/cd/B10501_01/appdev.920/a96624/10_objs.htm) based on a tables’ metadata that provide DDL/DML functionality. Type inheritance is possible, too, so that subtypes can use the supertypes’ functions.
 
 
## <a name="prerequisites"></a> 2. Prerequisites
 
- the generator's owner has to have the permission to see the table (for which the type shall be generated) and the data dictionary
- there has to be a numeric primary key on the table (index + constraint)
- the generator uses unique indexes on a table to create the type's constructors, so at least one is needed
- to support DML operations a sequence to generate the primary key is needed. By default, the sequence's name has to be <<TABLE_NAME>>_SEQ, but you can modify the generators' package specification (v_sequence_prefix  and v_sequence_suffix) to match your personal needs
- to lock rows, you need (per default) the following columns in your table (like the sequence pre- and suffix, also the column names can be modified in the package specification):

```
   - CREATED_USER    VARCHAR2(100)
   - CREATED_DATE    DATE
   - CHANGED_USER    VARCHAR2(100)
   - CHANGED_DATE    DATE
   - CHANGED_COUNTER NUMBER(12)
   - DELETED_DATE    DATE
```
 
## <a name="installation"></a> 3. Installation
 
1. Download content from "src"-folder
2. install package specification and body into your database
 
 
## <a name="additionalinformation"></a> 4. Additional information
 
- this generator works with function-based indexes as unique indexes
- the generated types are good for single record processing. Sometimes direct inserts are necessary (e.g. for batch processing) because of performance reasons. To use both modes we recommendinstalling the generated triggers into your database.
 
 
## <a name="license"></a> 5. License
 
Copyright (C) 2016 Joh. Berenberg, Gossler & Co. KG. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA or obtain a copy of the Licence at http://www.gnu.org/licenses/gpl-2.0.html.
