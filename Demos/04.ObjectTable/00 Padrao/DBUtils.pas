unit DBUtils;

interface

uses
  IBDatabase,
  JazzSessionIntf,
  JazzDataFactory,
  JazzMappingIntf,
  JazzValueTypeIntf;

type
  TProximoRegistro_Generator = class(TIntegerGenerator)
  private
    FObjectMeta: IObjectMeta;
  protected
    function Next(const Member: IMemberType): Boolean; override;
    procedure GenerateNext; override;
  end;

function NewSessionIBX(DataBase: string; User: string = 'sysdba'; Password:
  string = 'masterkey'; AConnection: TIBDatabase = nil): ISession;

implementation

uses
 JazzIBXMechanism,
 JazzSession,
 JazzPersister,
 JazzDatabaseMechanismIntf,
 JazzCriteriaIntf,
 ProximoRegistro_Intf,
 JazzCriteria,
 ProximoRegistro;

function NewSessionIBX(DataBase: string; User: string = 'sysdba'; Password:
  string = 'masterkey'; AConnection: TIBDatabase = nil): ISession;
var
  LConnection: TIBDatabase;
begin
  Result := NewSession(TIBXMechanism);
  if AConnection <> nil then
  begin
    LConnection:= AConnection;
    (ActiveSession.Mechanism as IDatabaseMechanism).Connection:= LConnection;
  end
  else
    LConnection:= TIBDatabase((ActiveSession.Mechanism as IDatabaseMechanism).Connection);

  with (ActiveSession.Mechanism as IDatabaseMechanism) do
  begin
    LConnection.SQLDialect := 3;
    LConnection.DatabaseName := DataBase;
    LConnection.Params.Values['lc_ctype'] := 'ISO8859_1';
    LConnection.Params.Values['user_name'] := User;
    LConnection.Params.Values['password'] := Password;
    Connected := True;
  end;
end;


{ TProximoRegistro_Generator }

procedure TProximoRegistro_Generator.GenerateNext;
var
  Criteria: ICriteria;
  ProximoRegistroList: IObjectListType;
  ProximoRegistro: IProximoRegistro;
begin
  Criteria := NewCriteria;
  Criteria.Add(ctEqualTo, 'NomeTabela', [FObjectMeta.TableName]);

  ProximoRegistroList := TProximoRegistro.NewList;
  ProximoRegistroList.BeginUpdate;
  try
    ActiveSession.Load(ProximoRegistroList, Criteria);
    if (ProximoRegistroList.Count = 0) then
    begin
      ProximoRegistro := (ProximoRegistroList.Add as IProximoRegistro);
      ProximoRegistro.Tabela := FObjectMeta.TableName;
    end
    else
      ProximoRegistro := (ProximoRegistroList.Items[0] as IProximoRegistro);

    ProximoRegistro.UltimoRegistro := ProximoRegistro.UltimoRegistro + 1;
    Last := ProximoRegistro.UltimoRegistro;
    ActiveSession.Save(ProximoRegistro);
  finally
    ProximoRegistroList.EndUpdate;
  end;
end;

function TProximoRegistro_Generator.Next(const Member: IMemberType): Boolean;
var
  sClassName: string;
begin
  sClassName := (Member.Owner as IValueType).Implementor.ClassName;
  FObjectMeta := ActiveSession.Mapping.Find(sClassName);
  Result := inherited Next(Member);
end;

end.
