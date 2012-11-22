unit Sistema_Impl;

interface

uses
  Classes, Sistema_Intf, JazzValueTypeIntf, JazzValueType;

type
  TCAP_Sistema = class(TObjectType, ICAP_Sistema)
  private
    FDescricao: IStringType;
    FID: IIntegerType;
    FComentario: IMemoType;
    FFoto: IBlobType;
    function GetDescricao: string;
    function GetID: Integer;
    procedure SetDescricao(const Value: string);
    procedure SetID(const Value: Integer);

    function GetFoto: TStream;
    procedure SetFoto(Value: TStream);
    function GetComentario: TStrings;
    procedure SetComentario(Value: TStrings);
  protected
    procedure InitInstance; override;
  published
    property ID: Integer read GetID write SetID;
    property Descricao: String read GetDescricao write SetDescricao;
    property Comentario: TStrings read GetComentario write SetComentario;
    property Foto: TStream read GetFoto write SetFoto;
  end;

implementation

uses
  JazzTypeInfo;

{ TCAP_Sistema }

function TCAP_Sistema.GetComentario: TStrings;
begin
  Result := FComentario.Value;
end;

function TCAP_Sistema.GetDescricao: string;
begin
  Result := FDescricao.Value;
end;

function TCAP_Sistema.GetFoto: TStream;
begin
  Result := FFoto.Value;
end;

function TCAP_Sistema.GetID: Integer;
begin
  Result := FID.Value;
end;

procedure TCAP_Sistema.InitInstance;
begin
  inherited;
  AddMember(FDescricao, 'Descricao', TStringType);
  AddMember(FID, 'ID', TIntegerType);
  AddMember(FComentario, 'Comentario', TMemoType); 
  AddMember(FFoto, 'Foto', TBlobType);
end;

procedure TCAP_Sistema.SetComentario(Value: TStrings);
begin
  FComentario.Value := Value;
end;

procedure TCAP_Sistema.SetDescricao(const Value: string);
begin
  FDescricao.Value := Value;
end;

procedure TCAP_Sistema.SetFoto(Value: TStream);
begin
  FFoto.Value := Value;
end;

procedure TCAP_Sistema.SetID(const Value: Integer);
begin
  FID.Value := Value;
end;

initialization
  RegisterType(ICAP_Sistema, 'CAP_SISTEMA', TCAP_Sistema);

end.
