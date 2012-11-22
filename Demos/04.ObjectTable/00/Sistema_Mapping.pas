unit Sistema_Mapping;

interface

uses
  JazzMapping, JazzMappingIntf, JazzDataFactory, JazzValueTypeIntf;

type
  TCAP_SistemaMapping = class(TMappingLoader)
  protected
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

uses
  Sistema_Impl, Dialogs, DBUtils;

{ TCAP_SistemaMapping }

procedure TCAP_SistemaMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject(TCAP_Sistema, 'CAP_SISTEMA') do
    begin
      with Add('ID', 'ID_SISTEMA', itInteger) do
      begin
        IsOID := True;
        GeneratorClass := TCAP_NextIDGenerator;//TAscendingGenerator;
      end;
      with Add('Descricao', 'DESCRICAO', itString, 40) do
        Required := True;
      with Add('Comentario', 'COMENTARIO', itMemo) do
        LazyLoad := False;
      with Add('Foto', 'FOTO', itImage) do
        LazyLoad := False
    end;
  end;
end;

initialization
  RegisterMapping(TCAP_SistemaMapping);

end.
