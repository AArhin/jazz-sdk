unit Pessoa_Intf;

interface

uses
  Classes, JazzValueTypeIntf;

type
  IPessoa = interface(IObjectType)
    ['{5AD13B1B-B8D1-43E2-950A-666FA2E2B469}']
    function GetCodigoPessoa: Integer;
    function GetComentarios: TStrings;
    function GetDataCadastro: TDateTime;
    function GetFoto: TStream;
    function GetNome: String;
    function GetTipoPessoa: String;
    procedure SetCodigoPessoa(const Value: Integer);
    procedure SetComentarios(const Value: TStrings);
    procedure SetDataCadastro(const Value: TDateTime);
    procedure SetFoto(const Value: TStream);
    procedure SetNome(const Value: String);
    procedure SetTipoPessoa(const Value: String);

    property CodigoPessoa: Integer read GetCodigoPessoa write SetCodigoPessoa;
    property Nome: String read GetNome write SetNome;
    property TipoPessoa: String read GetTipoPessoa write SetTipoPessoa;
    property DataCadastro: TDateTime read GetDataCadastro write SetDataCadastro;
    property Comentarios: TStrings read GetComentarios write SetComentarios;
    property Foto: TStream read GetFoto write SetFoto;
  end;

implementation

end.
