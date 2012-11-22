unit AppConsts;

interface

{.$DEFINE LANG_EN}
{$DEFINE LANG_ptBR}

const
  VK_U_UPPER = 85;
  VK_U_LOWER = 117;
  ADOmdbConnectionStr = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;Persist Security Info=False';

{$IFDEF LANG_EN}
  SDeleteSelection = 'Delete Selection?'#13#10'%s';
  SDeleteAll = 'Delete All?';

  S_Yes = 'Yes';
  S_No = 'No';
  S_True = 'True';
  S_False = 'False';
{$ENDIF}

{$IFDEF LANG_ptBR}
  SDeleteSelection = 'Excluir Selecionados?'#13#10'%s';
  SDeleteAll = 'Excluir Todos?';

  S_Yes = 'Sim';
  S_No = 'Não';
  S_True = 'Verdadeiro';
  S_False = 'False';
{$ENDIF}

implementation

end.
