
unit Sistema_Intf;

interface

uses
  Classes, JazzValueTypeIntf;

type
  ICAP_Sistema = interface(IObjectType)
    ['{22BE9029-20F0-4F1C-90DE-1C617B57CEF5}']
    function GetID: Integer;
    procedure SetID(const Value: Integer);
    function GetDescricao: String;
    procedure SetDescricao(const Value: String);

    function GetFoto: TStream;
    procedure SetFoto(Value: TStream);
    function GetComentario: TStrings;
    procedure SetComentario(Value: TStrings);

    property ID: Integer read GetID write SetID;
    property Descricao: String read GetDescricao write SetDescricao;
    property Comentario: TStrings read GetComentario write SetComentario;
    property Foto: TStream read GetFoto write SetFoto;
  end;

implementation

end.
