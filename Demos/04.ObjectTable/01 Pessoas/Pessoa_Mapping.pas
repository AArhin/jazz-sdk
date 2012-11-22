unit Pessoa_Mapping;

interface

uses
  JazzMapping, JazzMappingIntf;

type
  TPessoa_Mapping = class(TMappingLoader)

  protected
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

uses DBUtils;

{ TPessoa_Mapping }

procedure TPessoa_Mapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject('TPessoa', 'PAD_PESSOA') do
    begin
      with Add('CodigoPessoa', 'CODIGOPESSOA', itInteger) do
      begin
        IsOID := True;
        GeneratorClass := TProximoRegistro_Generator;
      end;
      with Add('Nome', 'NOME', itString, 100) do
        Required := True;
      with Add('TipoPessoa', 'TIPOPESSOA', itString, 1) do
        Required := True;
      Add('DataCadastro', 'DATACADASTRO', itTimeStamp);
      with Add('Comentario', 'COMENTARIO', itMemo) do
        LazyLoad := False;
      with Add('Foto', 'FOTO', itImage) do
        LazyLoad := False
    end;
  end;
end;

initialization
  RegisterMapping(TPessoa_Mapping);

end.
