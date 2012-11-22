unit ClassesIntf;

interface

uses
  Classes,
  JazzValueTypeIntf;

type
  IPessoa = interface(IObjectType)
    ['{159AEE13-8723-4491-976E-D9AE95B048F5}']
    function GetDataNascimento: TDateTime;
    function GetID: string;
    function GetNome: string;
    procedure SetDataNascimento(Value: TDateTime);
    procedure SetID(const Value: string);
    procedure SetNome(const Value: string);
    function GetEnderecoList: IObjectListType;
    procedure SetEnderecoList(const Value: IObjectListType);
    function GetFoto: TStream;
    procedure SetFoto(Value: TStream);

    property ID: string read GetID write SetID;
    property Nome: string read GetNome write SetNome;
    property DataNascimento: TDateTime read GetDataNascimento write SetDataNascimento;
    property EnderecoList: IObjectListType read GetEnderecoList write SetEnderecoList;
    property Foto: TStream read GetFoto write SetFoto;
  end;

  IEndereco = interface(IObjectType)
    ['{695BCCB9-FDD3-4A60-BD10-4727945D7918}']
    function GetBairro: string;
    function GetCidade: string;
    function GetID: string;
    function GetNumero: Integer;
    function GetRua: string;
    function GetUF: string;
    procedure SetBairro(const Value: string);
    procedure SetCidade(const Value: string);
    procedure SetID(const Value: string);
    procedure SetNumero(Value: Integer);
    procedure SetRua(const Value: string);
    procedure SetUF(const Value: string);
    function GetIdOwner: string;
    procedure SetIdOwner(const Value: string);

    property ID: string read GetID write SetID;
    property Rua: string read GetRua write SetRua;
    property Numero: Integer read GetNumero write SetNumero;
    property Bairro: string read GetBairro write SetBairro;
    property Cidade: string read GetCidade write SetCidade;
    property UF: string read GetUF write SetUF;
    property IdOwner: string read GetIdOwner write SetIdOwner;
  end;


implementation

end.
