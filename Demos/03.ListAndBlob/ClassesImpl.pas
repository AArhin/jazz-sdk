unit ClassesImpl;

interface

uses
  Classes,
  ClassesIntf, 
  JazzValueTypeIntf,
  JazzValueType;

type
  TPessoa = class(TObjectType, IPessoa)
  private
    FID: IStringType;
    FNome: IStringType;
    FDataNascimento: IDateType;
    FEnderecoList: IObjectListType;
    FFoto: IBlobType;
  protected
    function GetDataNascimento: TDateTime;
    function GetEnderecoList: IObjectListType;
    function GetFoto: TStream;
    function GetID: string;
    function GetNome: string;
    procedure SetDataNascimento(Value: TDateTime);
    procedure SetEnderecoList(const Value: IObjectListType);
    procedure SetFoto(Value: TStream);
    procedure SetID(const Value: string);
    procedure SetNome(const Value: string);

    procedure ValidarNome(OldValue: string; var NewValue: string);
    procedure InitInstance; override;
  public
    property ID: string read GetID write SetID;
    property Nome: string read GetNome write SetNome;
    property DataNascimento: TDateTime read GetDataNascimento write SetDataNascimento;
    property EnderecoList: IObjectListType read GetEnderecoList write SetEnderecoList;
    property Foto: TStream read GetFoto write SetFoto;
  end;

  TEndereco = class(TObjectType, IEndereco)
  private
    FBairro: IStringType;
    FCidade: IStringType;
    FID: IStringType;
    FIdOwner: IStringType;
    FNumero: IIntegerType;
    FRua: IStringType;
    FUF: IStringType;
  protected
    function GetBairro: string;
    function GetCidade: string;
    function GetID: string;
    function GetIdOwner: string;
    function GetNumero: Integer;
    function GetRua: string;
    function GetUF: string;
    procedure SetBairro(const Value: string);
    procedure SetCidade(const Value: string);
    procedure SetID(const Value: string);
    procedure SetIdOwner(const Value: string);
    procedure SetNumero(Value: Integer);
    procedure SetRua(const Value: string);
    procedure SetUF(const Value: string);

    procedure InitInstance; override;
  public
    property ID: string read GetID write SetID;
    property Rua: string read GetRua write SetRua;
    property Numero: Integer read GetNumero write SetNumero;
    property Bairro: string read GetBairro write SetBairro;
    property Cidade: string read GetCidade write SetCidade;
    property UF: string read GetUF write SetUF;
    property IdOwner: string read GetIdOwner write SetIdOwner;
  end;

implementation

function TPessoa.GetID: string;
begin
  Result:= FID.Value;
end;

procedure TPessoa.SetID(const Value: string);
begin
  FID.Value:= Value;
end;

function TPessoa.GetNome: string;
begin
  Result:= FNome.Value;
end;

procedure TPessoa.SetNome(const Value: string);
begin
  FNome.Value:= Value;
end;

function TPessoa.GetDataNascimento: TDateTime;
begin
  Result:= FDataNascimento.Value;
end;

procedure TPessoa.SetDataNascimento(Value: TDateTime);
begin
  FDataNascimento.Value:= Value;
end;

procedure TPessoa.InitInstance;
begin
  inherited;
  AddMember(FID, 'ID', TStringType); // do not localize
  AddMember(FNome, 'Nome', TStringType); // do not localize
  AddMember(FDataNascimento, 'DataNascimento', TDateType); // do not localize
  AddMember(FEnderecoList, 'EnderecoList', TObjectListType, TEndereco); // do not localize
  AddMember(FFoto, 'Foto', TBlobType); // do not localize

  FNome.OnSetValue:= ValidarNome;
end;

function TEndereco.GetID: string;
begin
  Result:= FID.Value;
end;

procedure TEndereco.SetID(const Value: string);
begin
  FID.Value:= Value;
end;

function TEndereco.GetRua: string;
begin
  Result:= FRua.Value;
end;

procedure TEndereco.SetRua(const Value: string);
begin
  FRua.Value:= Value;
end;

function TEndereco.GetNumero: Integer;
begin
  Result:= FNumero.Value;
end;

procedure TEndereco.SetNumero(Value: Integer);
begin
  FNumero.Value:= Value;
end;

function TEndereco.GetBairro: string;
begin
  Result:= FBairro.Value;
end;

procedure TEndereco.SetBairro(const Value: string);
begin
  FBairro.Value:= Value;
end;

function TEndereco.GetCidade: string;
begin
  Result:= FCidade.Value;
end;

procedure TEndereco.SetCidade(const Value: string);
begin
  FCidade.Value:= Value;
end;

function TEndereco.GetUF: string;
begin
  Result:= FUF.Value;
end;

procedure TEndereco.SetUF(const Value: string);
begin
  FUF.Value:= Value;
end;

procedure TEndereco.InitInstance;
begin
  inherited;
  AddMember(FID, 'ID', TStringType);          // do not localize
  AddMember(FRua, 'Rua', TStringType);        // do not localize
  AddMember(FNumero, 'Numero', TIntegerType); // do not localize
  AddMember(FBairro, 'Bairro', TStringType);  // do not localize
  AddMember(FCidade, 'Cidade', TStringType);  // do not localize
  AddMember(FUF, 'UF', TStringType);          // do not localize
  AddMember(FIdOwner, 'IdOwner', TStringType);// do not localize
end;

function TEndereco.GetIdOwner: string;
begin
  Result:= FIdOwner.Value;
end;

procedure TEndereco.SetIdOwner(const Value: string);
begin
  FIdOwner.Value:= Value;
end;

function TPessoa.GetEnderecoList: IObjectListType;
begin
  Result:= FEnderecoList;
end;

procedure TPessoa.SetEnderecoList(const Value: IObjectListType);
begin
  FEnderecoList:= Value;
end;

procedure TPessoa.ValidarNome(OldValue: string; var NewValue: string);
begin
  if not State.Loaded and not State.Persisted and not Loading then
    NewValue:= '[' + NewValue + ']';
end;

function TPessoa.GetFoto: TStream;
begin
  Result:= FFoto.Value;
end;

procedure TPessoa.SetFoto(Value: TStream);
begin
  FFoto.Value:= Value;
end;

end.



