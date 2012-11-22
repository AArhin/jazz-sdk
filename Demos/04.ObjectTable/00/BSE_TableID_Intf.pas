unit BSE_TableID_Intf;

interface

uses
  Classes, JazzValueTypeIntf;

type
  IBSE_TableID = interface(IObjectType)
    ['{D19C252A-010A-4E04-85F1-68B02881BF92}']
    function GetNomeTabela: String;
    function GetUltimoRegistro: Integer;
    procedure SetNomeTabela(const Value: String);
    procedure SetUltimoRegistro(const Value: Integer);

    property NomeTabela: String read GetNomeTabela write SetNomeTabela;
    property UltimoRegistro: Integer read GetUltimoRegistro write SetUltimoRegistro;
  end;

implementation

end.
