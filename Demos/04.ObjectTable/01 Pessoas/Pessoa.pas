unit Pessoa;

interface

uses
  Classes, JazzValueType, JazzValueTypeIntf, Pessoa_Intf;

type
  TPessoa = class(TObjectType, IPessoa)
  private
    FCodigoPessoa: IIntegerType;
    FNome: IStringType;
    FTipoPessoa: IStringType;
    FDataCadastro: IDateType;
    FComentario: IMemoType;
    FFoto: IBlobType;

    function GetCodigoPessoa: Integer;
    procedure SetCodigoPessoa(const Value: Integer);
    function GetNome: String;
    procedure SetNome(const Value: String);
    function GetTipoPessoa: String;
    procedure SetTipoPessoa(const Value: String);
    function GetDataCadastro: TDateTime;
    procedure SetDataCadastro(const Value: TDateTime);
    function GetComentarios: TStrings;
    procedure SetComentarios(const Value: TStrings);
    function GetFoto: TStream;
    procedure SetFoto(const Value: TStream);
  protected
    procedure InitInstance; override;
  public
    property CodigoPessoa: Integer read GetCodigoPessoa write SetCodigoPessoa;
    property Nome: String read GetNome write SetNome;
    property TipoPessoa: String read GetTipoPessoa write SetTipoPessoa;
    property DataCadastro: TDateTime read GetDataCadastro write SetDataCadastro;
    property Comentarios: TStrings read GetComentarios write SetComentarios;
    property Foto: TStream read GetFoto write SetFoto;
  end;

implementation

uses
  JazzTypeInfo;

{ TPessoa }

function TPessoa.GetCodigoPessoa: Integer;
begin
  Result := FCodigoPessoa.Value;
end;

function TPessoa.GetComentarios: TStrings;
begin
  Result := FComentario.Value;
end;

function TPessoa.GetDataCadastro: TDateTime;
begin
  Result := FDataCadastro.Value;
end;

function TPessoa.GetFoto: TStream;
begin
  Result := FFoto.Value;
end;

function TPessoa.GetNome: String;
begin
  Result := FNome.Value;
end;

function TPessoa.GetTipoPessoa: String;
begin
  Result := FTipoPessoa.Value;
end;

procedure TPessoa.InitInstance;
begin
  inherited;
  AddMember(FCodigoPessoa, 'CodigoPessoa', TIntegerType);
  AddMember(FNome, 'Nome', TStringType);
  AddMember(FTipoPessoa, 'TipoPessoa', TStringType);
  AddMember(FDataCadastro, 'DataCadastro', TDateType);
  AddMember(FComentario, 'Comentario', TMemoType);
  AddMember(FFoto, 'Foto', TBlobType);
end;

procedure TPessoa.SetCodigoPessoa(const Value: Integer);
begin
  FCodigoPessoa.Value := Value;
end;

procedure TPessoa.SetComentarios(const Value: TStrings);
begin
  FComentario.Value := Value;
end;

procedure TPessoa.SetDataCadastro(const Value: TDateTime);
begin
  FDataCadastro.Value := Value;
end;

procedure TPessoa.SetFoto(const Value: TStream);
begin
  FFoto.Value := Value;
end;

procedure TPessoa.SetNome(const Value: String);
begin
  FNome.Value := Value;
end;

procedure TPessoa.SetTipoPessoa(const Value: String);
begin
  FTipoPessoa.Value := Value;
end;

initialization
  RegisterType(IPessoa, 'TPessoa', TPessoa);
  
end.
