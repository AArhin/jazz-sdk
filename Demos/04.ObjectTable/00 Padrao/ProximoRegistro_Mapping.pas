unit ProximoRegistro_Mapping;

interface

uses
  JazzMapping, JazzMappingIntf;

type
  TProximoRegistro_Mapping = class(TMappingLoader)
  protected
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

{ TProximoRegistro_Mapping }

procedure TProximoRegistro_Mapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject('TProximoRegistro', 'PAD_PROXIMOREGISTRO') do
    begin
      with Add('Tabela', 'TABELA', itString, 60) do
        Required := True;
      Add('UltimoRegistro', 'ULTIMOREGISTRO', itInteger);
    end;
  end;
end;

initialization
  RegisterMapping(TProximoRegistro_Mapping);

end.
