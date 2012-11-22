unit BSE_TableID_Impl;

interface

uses
  Classes, BSE_TableID_Intf, JazzValueTypeIntf, JazzValueType;

type
  TBSE_TableID = class(TObjectType, IBSE_TableID)
  private
    FNomeTabela: IStringType;
    FUltimoRegistro: IIntegerType;
    function GetNomeTabela: String;
    function GetUltimoRegistro: Integer;
    procedure SetNomeTabela(const Value: String);
    procedure SetUltimoRegistro(const Value: Integer);
  protected
    procedure InitInstance; override;
  public
    property NomeTabela: String read GetNomeTabela write SetNomeTabela;
    property UltimoRegistro: Integer read GetUltimoRegistro write SetUltimoRegistro;
  end;

implementation

{ IBSE_TableID }

function TBSE_TableID.GetNomeTabela: String;
begin
  Result := FNomeTabela.Value;
end;

function TBSE_TableID.GetUltimoRegistro: Integer;
begin
  Result := FUltimoRegistro.Value;
end;

procedure TBSE_TableID.InitInstance;
begin
  inherited;
  AddMember(FNomeTabela, 'NomeTabela', TStringType);
  AddMember(FUltimoRegistro, 'UltimoRegistro', TIntegerType);
end;

procedure TBSE_TableID.SetNomeTabela(const Value: String);
begin
  FNomeTabela.Value := Value;
end;

procedure TBSE_TableID.SetUltimoRegistro(const Value: Integer);
begin
  FUltimoRegistro.Value := Value;
end;

end.
