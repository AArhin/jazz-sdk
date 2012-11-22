unit SetupDBConnection;

{$I Jazz.inc}

interface

uses SqlExpr;

var
  DatabasePath: string = '..\00.Common\DB\JazzDBDemos.fdb';
  DatabaseLogin: string = 'sysdba';
  DatabasePassword: string = 'masterkey';

  //TODO: verificar versão da "dbexpint.dll" no delphi 8 e 2005
{$IFDEF DELPHI10}
 DatabaseLibraryName: string = 'dbxint30.dll';
{$ELSE}
   {$IFDEF DELPHI6}
     DatabaseLibraryName: string = 'dbexpint.dll';
   {$ELSE}
      {$MESSAGE FATAL 'Delphi 5 ou anterior não suportado'}
   {$ENDIF}
{$ENDIF} 
  
procedure SetupDBXConnection(const Connection: TObject);

implementation

procedure SetupDBXConnection(const Connection: TObject);
begin
  with Connection as TSQLConnection do
  begin
    ConnectionName:= 'JazzDBDemos';
    DriverName:= 'Interbase';
    GetDriverFunc:= 'getSQLDriverINTERBASE';
    LibraryName:= DatabaseLibraryName;
    LoginPrompt:= False;
    Params.Clear;
    Params.Add('DriverName=Interbase');
    Params.Add('Database=' + DatabasePath);
    Params.Add('RoleName=RoleName');
    Params.Add('User_Name=' + DatabaseLogin);
    Params.Add('Password=' + DatabasePassword);
    Params.Add('ServerCharSet=WIN1252');
    Params.Add('SQLDialect=3');
    Params.Add('BlobSize=-1');
    Params.Add('CommitRetain=False');
    Params.Add('WaitOnLocks=False');
    Params.Add('ErrorResourceFile=');
    Params.Add('LocaleCode=0000');
    Params.Add('Interbase TransIsolation=ReadCommited');
    Params.Add('Trim Char=True');
    VendorLib:= 'gds32.dll';
  end;
end;

end.

