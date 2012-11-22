unit JazzFirebirdConsts;

interface

uses
  SysUtils,
  JazzDatabaseMechanismIntf,
  JazzMappingIntf;

procedure SetFirebirdColumnsType(const DatabaseDriver: IDatabaseDriver);
function GetDomainStatement(ColumnType: TItemType; var SQL: string): boolean;


const
  SColumnSizeDef = '(%d)';
  SPrecisionScale = '(%d,%d)';

  ColumnOrder = 0;
  ColumnName = 1;
  ColumnTypeCode = 2;
  ColumnType = 3;
  ColumnSize = 4;
  ColumnPrecision = 5;
  ColumnScale = 6;
  ColumnNotNull = 7;
  ColumnPrimaryKey = 8;

  SPrecisionColumnType = 'NUMERIC';
//  SPrecisionColumnType = 'DECIMAL';
  
  SQLFieldsSchema =
  // Order, Name, Type Code, Type, Size, Precision, Not Null, Primary Key 
    'SELECT DISTINCT ' +
    '    A.RDB$FIELD_POSITION AS "Order", ' +
    '    A.RDB$FIELD_NAME AS "Name", ' +
    '    B.RDB$FIELD_TYPE AS "Type Code", ' +
    '    CASE ' +
//    '        WHEN B.RDB$FIELD_PRECISION > 0 THEN ''NUMERIC(''||CAST(B.RDB$FIELD_PRECISION AS VARCHAR(2))||'',''||CAST(B.RDB$FIELD_SCALE*-1 AS VARCHAR(2))||'')''' +
    '        WHEN C.RDB$TYPE_NAME=''LONG''  THEN ''' + SPrecisionColumnType + ''' ' +
    '        WHEN C.RDB$TYPE_NAME=''LONG''  THEN ''INTEGER'' ' +
    '        WHEN C.RDB$TYPE_NAME=''SHORT'' THEN ''SMALLINT'' ' +
    '        WHEN C.RDB$TYPE_NAME=''VARYING'' THEN ''VARCHAR'' ' +
    '        WHEN C.RDB$TYPE_NAME=''TEXT'' THEN ''CHAR'' ' +
    '        WHEN C.RDB$TYPE_NAME=''BLOB'' THEN ''BLOB SUB_TYPE ''||CAST(B.RDB$FIELD_SUB_TYPE AS CHAR(1)) ' +
    '    ELSE ' +
    '        C.RDB$TYPE_NAME ' +
    '    END AS "Type", ' +
    '    CASE ' +
    '        WHEN C.RDB$TYPE_NAME=''INT64'' THEN 0 ' +
    '        WHEN B.RDB$FIELD_PRECISION > 0 THEN CAST(B.RDB$FIELD_PRECISION AS VARCHAR(2))||'',''||CAST(B.RDB$FIELD_SCALE*-1 AS VARCHAR(2)) ' +
    '        WHEN C.RDB$TYPE_NAME=''VARYING'' THEN B.RDB$CHARACTER_LENGTH ' +
    '        WHEN C.RDB$TYPE_NAME=''TEXT'' THEN B.RDB$CHARACTER_LENGTH ' +
    '        WHEN C.RDB$TYPE_NAME=''BLOB'' THEN 1 ' +
    '    ELSE 0 ' +
    '    END AS "Size", ' +
    '    B.RDB$FIELD_PRECISION*-1 AS "Precision", ' +
    '    B.RDB$FIELD_SCALE*-1 AS "Scale", ' +
    '    CASE ' +
    '        WHEN A.RDB$NULL_FLAG IS NULL  THEN ''False'' ' +
    '    ELSE  ''True'' ' +
    '    END AS "Not Null", ' +
    '    CASE ' +
    '      WHEN F.RDB$FIELD_NAME IS NULL THEN ''False'' ' +
    '    ELSE ' +
    '      ''True'' ' +
    '    END AS "Primary Key" ' +
    'FROM ' +
    '    RDB$RELATION_FIELDS A ' +
    '    LEFT JOIN RDB$FIELDS ' +
    '        B ON A.RDB$FIELD_SOURCE=B.RDB$FIELD_NAME ' +
    '    LEFT JOIN RDB$TYPES ' +
    '        C ON C.RDB$FIELD_NAME=''RDB$FIELD_TYPE'' AND B.RDB$FIELD_TYPE=C.RDB$TYPE ' +
    '    LEFT JOIN RDB$RELATION_CONSTRAINTS ' +
    '        E ON A.RDB$RELATION_NAME=E.RDB$RELATION_NAME AND E.RDB$CONSTRAINT_TYPE=''PRIMARY KEY'' ' +
    '    LEFT JOIN RDB$INDEX_SEGMENTS ' +
    '        F ON E.RDB$INDEX_NAME=F.RDB$INDEX_NAME AND A.RDB$FIELD_NAME=F.RDB$FIELD_NAME ' +
    'WHERE A.RDB$RELATION_NAME = ''%s'' ';


  SQLForeignKeys = 
    'SELECT '+
    '    R.RDB$CONSTRAINT_NAME AS FK_NAME, '+
    '    R.RDB$RELATION_NAME  AS TABLE_NAME, '+
    '    I.RDB$FIELD_NAME AS FIELD_NAME, '+
    '    IDX.RDB$FOREIGN_KEY AS REF_KEY, '+
    '    IDX1.RDB$RELATION_NAME AS REF_TABLE, '+
    '    I1.RDB$FIELD_NAME AS REF_FIELDNAME, '+
    '    R.RDB$CONSTRAINT_TYPE AS CONSTRAINT_TYPE '+
    'FROM RDB$RELATION_CONSTRAINTS R '+
    '  JOIN RDB$INDEX_SEGMENTS I ON (R.RDB$INDEX_NAME = I.RDB$INDEX_NAME) '+
    '  JOIN RDB$INDICES IDX ON (R.RDB$CONSTRAINT_NAME = IDX.RDB$INDEX_NAME) '+
    '  JOIN RDB$INDICES IDX1 ON (IDX.RDB$FOREIGN_KEY = IDX1.RDB$INDEX_NAME) '+
    '  JOIN RDB$INDEX_SEGMENTS I1 ON (IDX.RDB$FOREIGN_KEY = I1.RDB$INDEX_NAME) '+
    'WHERE '+
    '    (R.RDB$RELATION_NAME = ''%s'') AND '+
    '    (R.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'') ';

  SQLIndexesSchema =     
    'SELECT '+
    '    R.RDB$CONSTRAINT_NAME AS FK_NAME, '+
    '    R.RDB$RELATION_NAME  AS TABLE_NAME, '+
    '    I.RDB$FIELD_NAME AS FIELD_NAME, '+
    '    IDX.RDB$FOREIGN_KEY AS REF_KEY, '+
    '    IDX1.RDB$RELATION_NAME AS REF_TABLE, '+
    '    I1.RDB$FIELD_NAME AS REF_FIELDNAME, '+
    '    R.RDB$CONSTRAINT_TYPE AS CONSTRAINT_TYPE '+
    'FROM RDB$RELATION_CONSTRAINTS R '+
    '  JOIN RDB$INDEX_SEGMENTS I ON (R.RDB$INDEX_NAME = I.RDB$INDEX_NAME) '+
    '  JOIN RDB$INDICES IDX ON (R.RDB$CONSTRAINT_NAME = IDX.RDB$INDEX_NAME) '+
    '  JOIN RDB$INDICES IDX1 ON (IDX.RDB$FOREIGN_KEY = IDX1.RDB$INDEX_NAME) '+
    '  JOIN RDB$INDEX_SEGMENTS I1 ON (IDX.RDB$FOREIGN_KEY = I1.RDB$INDEX_NAME) '+
    'WHERE '+
    '    (R.RDB$RELATION_NAME = ''%s'') AND '+
    '    (R.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'') ';

  SQLPrimaryKeysSchema =
    'SELECT '+
    '  R.RDB$CONSTRAINT_TYPE AS KEY_TYPE, '+
    '  I.RDB$FIELD_NAME AS FIELD_NAME '+
    'FROM RDB$RELATION_CONSTRAINTS R '+
    'JOIN RDB$INDEX_SEGMENTS I ON (R.RDB$INDEX_NAME = I.RDB$INDEX_NAME) '+
    'WHERE (R.RDB$RELATION_NAME = ''%s'') AND '+
    '  (R.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'') ';
   
    
implementation

procedure SetFirebirdColumnsType(const DatabaseDriver: IDatabaseDriver);
begin
  with DatabaseDriver do
  begin
    SetColumnTypeFor(itAutoInc   , 'INTEGER'         );
    SetColumnTypeFor(itBlob      , 'BLOB SUB_TYPE 0' );
    SetColumnTypeFor(itBoolean   , 'INTEGER'         );
    SetColumnTypeFor(itChar      , 'CHAR(1)'         );
    SetColumnTypeFor(itCurrency  , SPrecisionColumnType + '(18,2)');
    SetColumnTypeFor(itDate      , 'DATE'            );
    SetColumnTypeFor(itDateTime  , 'TIMESTAMP'       );
    SetColumnTypeFor(itFloat     , 'DOUBLE PRECISION');
    SetColumnTypeFor(itImage     , 'BLOB SUB_TYPE -1');
    SetColumnTypeFor(itInteger   , 'INTEGER'         );
    SetColumnTypeFor(itLongInt   , 'BIGINT'          );
    SetColumnTypeFor(itMemo      , 'BLOB SUB_TYPE 1' );
    SetColumnTypeFor(itSmallInt  , 'SMALLINT'        );
    SetColumnTypeFor(itString    , 'VARCHAR(%d)'     );
    SetColumnTypeFor(itTime      , 'TIME'            );
    SetColumnTypeFor(itTimeStamp , 'TIMESTAMP'       );
    SetColumnTypeFor(itWideString, 'VARCHAR(%d)'     );
  end;
end;

function GetDomainStatement(ColumnType: TItemType; var SQL: string): boolean;
const
  SDomain = 'CREATE DOMAIN %s AS %s';
begin
  case ColumnType of
    itAutoInc   : SQL:= Format(SDomain, ['AUTOINC', 'INTEGER']);
    itCurrency  : SQL:= Format(SDomain, ['CURRENCY', SPrecisionColumnType + '(18,2)']);
    itFloat     : SQL:= '';
    itInteger   : SQL:= '';
    itLongInt   : SQL:= '';
    itSmallInt  : SQL:= '';

    itString    : SQL:= '';
    itWideString: SQL:= '';

    itDate      : SQL:= '';
    itDateTime  : SQL:= Format(SDomain, ['DATETIME', 'TIMESTAMP']);
    itTime      : SQL:= '';
    itTimeStamp : SQL:= '';

    itBlob      : SQL:= Format(SDomain, ['BINARY', 'BLOB SUB_TYPE 0']);
    itMemo      : SQL:= Format(SDomain, ['MEMO', 'BLOB SUB_TYPE 1']);
    itImage     : SQL:= Format(SDomain, ['IMAGE', 'BLOB SUB_TYPE -1']);

    itBoolean   : SQL:= Format(SDomain, ['BOOLEAN', 'INTEGER']);
    itChar      : SQL:= '';
  end;
  Result:= SQL <> EmptyStr;
end;

{ Firebird DataTypes: Review DataTypes

BIGINT
BLOB - SegmentSize - SubType - charset
CHAR - length - charset - collate
DATE
DECIMAL - precision - scale
DOUBLE PRECISION
FLOAT
INTEGER
NUMERIC - precision - scale
SMALLINT
TIME
TIMESTAMP
VARCHAR - length - charset - collate

* Precision is the total number or maximum number of digits, both significant
  and fractional, that can appear in a column of these datatypes. The allowable
  range for precision is from 1 to a maximum of 18.
* Scale is the number of digits to the right of the decimal point that comprise
  the fractional portion of the number. The allowable range for scale is from
  zero to precision; in other words, scale must be less than or equal to
  precision.

* NUMERIC defines a datatype that holds a maximum of precision digits,
* DECIMAL defines a datatype that holds at least that number of digits,
  and possibly more.

* FLOAT specifies a single-precision, 32-bit datatype with a precision of
  approximately 7 decimal digits.
* DOUBLE PRECISION specifies a double-precision, 64-bit datatype with a
  precision of approximately 15 decimal digits.

  The precision of FLOAT and DOUBLE PRECISION is fixed by their size, but the
  scale is not, and you cannot control the formatting of the scale.

  If the value stored is outside of the range of the precision of the
  floating-point number, then it is stored only approximately, with its
  least-significant digits treated as zeros.
  For example, if the type is FLOAT, you are limited to 7 digits of precision.
  If you insert a 10-digit number 25.33333312 into the column, it is stored as
  25.33333.

* SMALLINT is a signed 16-bit integer (range –/+ 32,768)
* INTEGER is a signed 32-bit integer (range –/+ 2,147,483,648)
* BIGINT is a signed 64-bit integer (range -/+ 9,223,372,036,854,775,807)
}


end.

