program Pessoas;

uses
  Forms,
  Principal in 'Principal.pas' {frmPrincipal},
  Pessoa_Intf in 'Pessoa_Intf.pas',
  Pessoa in 'Pessoa.pas',
  Pessoa_Mapping in 'Pessoa_Mapping.pas',
  DBUtils in '..\00 Padrao\DBUtils.pas',
  ProximoRegistro_Intf in '..\00 Padrao\ProximoRegistro_Intf.pas',
  ProximoRegistro in '..\00 Padrao\ProximoRegistro.pas',
  ProximoRegistro_Mapping in '..\00 Padrao\ProximoRegistro_Mapping.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
