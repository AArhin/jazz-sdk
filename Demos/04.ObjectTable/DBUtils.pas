unit DBUtils;

interface

uses
  SysUtils, JazzDataFactory, JazzValueTypeIntf;

type
  TCAP_NextIDGenerator = class(TIntegerGenerator)
  private
    FClassName: String;
    FTableName: String;
  protected
    function Next(const Member: IMemberType): boolean; override;
    procedure GenerateNext; override;
  end;

implementation

uses
  JazzSession, JazzCriteriaIntf, JazzCriteria, BSE_TableID_Impl,
  BSE_TableID_Intf;

{ TCAP_NextIDGenerator }

procedure TCAP_NextIDGenerator.GenerateNext;
var
  Criteria: ICriteria;
  TableIDList: IObjectListType;
  TableID: IBSE_TableID;
begin
  Criteria := NewCriteria;
  Criteria.Add(ctEqualTo, 'NomeTabela', [FTableName]);

  TableIDList := TBSE_TableID.NewList;
  TableIDList.BeginUpdate;
  try
    ActiveSession.Load(TableIDList, Criteria);
    if (TableIDList.Count = 0) then
    begin
      TableID := (TableIDList.Add as IBSE_TableID);
      TableID.NomeTabela := FTableName;
    end
    else
      TableID := (TableIDList.Items[0] as IBSE_TableID);

    TableID.UltimoRegistro := TableID.UltimoRegistro + 1;
    Last := TableID.UltimoRegistro;
    ActiveSession.Save(TableID);
  finally
    TableIDList.EndUpdate;
  end;

end;

function TCAP_NextIDGenerator.Next(const Member: IMemberType): boolean;
begin
  FClassName := (Member.Owner as IValueType).Implementor.ClassName;
  FTableName := UpperCase(Copy(FClassName, 2, MaxInt));
  Result := inherited Next(Member);
end;

end.
