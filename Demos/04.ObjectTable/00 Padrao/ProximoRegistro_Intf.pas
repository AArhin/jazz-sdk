unit ProximoRegistro_Intf;

interface

uses
  JazzValueTypeIntf;

type
  IProximoRegistro = interface(IObjectType)
    ['{8B0364D3-2276-4262-8B33-649272F8C24F}']
    function GetTabela: String;
    procedure SetTabela(const Value: String);
    function GetUltimoRegistro: Integer;
    procedure SetUltimoRegistro(const Value: Integer);

    property Tabela: String read GetTabela write SetTabela;
    property UltimoRegistro: Integer read GetUltimoRegistro write SetUltimoRegistro;
  end;

implementation

end.
