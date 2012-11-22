unit BSE_TableID_Mapping;

interface

uses
  JazzMapping, JazzMappingIntf, JazzValueTypeIntf;

type
  TBSE_TableIDMapping = class(TMappingLoader)
  protected
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

uses
  BSE_TableID_Impl;

{ TBSE_TableIDMapping }

procedure TBSE_TableIDMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject(TBSE_TableID, 'BSE_TABELAID') do
    begin
      Add('NomeTabela', 'NOMETABELA', itString, 40);
      Add('UltimoRegistro', 'ULTIMOREGISTRO', itInteger);
    end;
  end;
end;

initialization
  RegisterMapping(TBSE_TableIDMapping);

end.
