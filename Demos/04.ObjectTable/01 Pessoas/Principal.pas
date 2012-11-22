unit Principal;

interface

uses
  JazzBaseTable,
  JazzDatabaseMechanismIntf,
  JazzObjectTable,

  Classes, ComCtrls, Controls, DB, DBCtrls, DBGrids, Dialogs, ExtCtrls, ExtDlgs,
  Forms, Graphics, Grids, Mask, Messages, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmPrincipal = class(TForm)
    pag: TPageControl;
    tabDados: TTabSheet;
    tabSQL: TTabSheet;
    mmoSQL: TMemo;
    dbg: TDBGrid;
    tblPessoa: TObjectTable;
    dsPessoa: TDataSource;
    Panel1: TPanel;
    DBNavigator: TDBNavigator;
    gbxPesquisa: TGroupBox;
    lblPesqNome: TLabel;
    edtPesqNome: TEdit;
    btnAbrir: TButton;
    gbxLocalizar: TGroupBox;
    edtLocNome: TEdit;
    lblLocNome: TLabel;
    btnLocalizar: TButton;
    gbxFiltro: TGroupBox;
    lblFiltroNome: TLabel;
    btnFiltrar: TButton;
    btnApplyChanges: TButton;
    btnCancelChanges: TButton;
    tblPessoaNome: TStringField;
    tblPessoaTipoPessoa: TStringField;
    tblPessoaCodigoPessoa: TIntegerField;
    DBMemo: TDBMemo;
    Bevel: TBevel;
    Panel2: TPanel;
    DBImage: TDBImage;
    btnImgAdiciona: TButton;
    btnImgRemover: TButton;
    tblPessoaComentario: TMemoField;
    tblPessoaFoto: TGraphicField;
    OpenPictureDialog: TOpenPictureDialog;
    cbxTipoPessoa: TComboBox;
    tblPessoaDataCadastro: TDateTimeField;
    procedure FormCreate(Sender: TObject);
    procedure btnAbrirClick(Sender: TObject);
    procedure btnLocalizarClick(Sender: TObject);
    procedure btnFiltrarClick(Sender: TObject);
    procedure btnApplyChangesClick(Sender: TObject);
    procedure btnCancelChangesClick(Sender: TObject);
    procedure tblPessoaAfterPost(DataSet: TDataSet);
    procedure AfterChanges(Sender: TObject);
    procedure tblPessoaFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure btnImgAdicionaClick(Sender: TObject);
    procedure btnImgRemoverClick(Sender: TObject);
    procedure cbxTipoPessoaChange(Sender: TObject);
    procedure dsPessoaStateChange(Sender: TObject);
    procedure tblPessoaAfterClose(DataSet: TDataSet);
    procedure tblPessoaNewRecord(DataSet: TDataSet);
  private
    { Private declarations }
    procedure DoExecuteStatement(const Mechanism: IDatabaseMechanism; const Command: string; const Params: TParams = nil);
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  DBUtils,
  JazzSession,
  JazzCriteriaIntf;

{$R *.dfm}

procedure TfrmPrincipal.btnAbrirClick(Sender: TObject);
begin
  if tblPessoa.Active then
  begin
    tblPessoa.Close;
    tblPessoa.ClearCriteria;
    btnAbrir.Caption := 'Abrir';
  end
  else
  begin
    if (Trim(edtPesqNome.Text) <> EmptyStr) then
      tblPessoa.Criteria.Add(ctLike, 'Nome', [edtPesqNome.Text + '%']);

    tblPessoa.Criteria.AddOrderBy('CodigoPessoa');
    try
      tblPessoa.Open;
    finally
      btnAbrir.Caption := 'Fechar';
    end;
  end;

  edtPesqNome.Enabled := not tblPessoa.Active;
  edtLocNome.Enabled := tblPessoa.Active;
  btnLocalizar.Enabled := tblPessoa.Active;
  cbxTipoPessoa.Enabled := tblPessoa.Active;
  btnFiltrar.Enabled := tblPessoa.Active;
end;

procedure TfrmPrincipal.btnApplyChangesClick(Sender: TObject);
begin
  tblPessoa.ApplyChanges;
end;

procedure TfrmPrincipal.btnCancelChangesClick(Sender: TObject);
begin
  tblPessoa.CancelChanges;
end;

procedure TfrmPrincipal.btnFiltrarClick(Sender: TObject);
begin
  if tblPessoa.Filtered then
  begin
    tblPessoa.Filtered := False;
    btnFiltrar.Caption := 'Filtrar';
  end
  else
  begin
    tblPessoa.Filtered := True;
    btnFiltrar.Caption := 'Cancelar';
  end;
end;

procedure TfrmPrincipal.btnImgAdicionaClick(Sender: TObject);
begin
  if not (tblPessoa.State in dsEditModes) then
    tblPessoa.Edit;
  try
    if OpenPictureDialog.Execute then
      tblPessoaFoto.LoadFromFile(OpenPictureDialog.FileName);
  finally
    tblPessoa.Post;
  end;
end;

procedure TfrmPrincipal.btnImgRemoverClick(Sender: TObject);
begin
  if not (tblPessoa.State in dsEditModes) then
    tblPessoa.Edit;
  try
    tblPessoaFoto.Clear;
  finally
    tblPessoa.Post;
  end;
end;

procedure TfrmPrincipal.btnLocalizarClick(Sender: TObject);
begin
  tblPessoa.Locate('Nome', edtLocNome.Text, []);
end;

procedure TfrmPrincipal.cbxTipoPessoaChange(Sender: TObject);
begin
  if tblPessoa.Filtered then tblPessoa.First;
end;

procedure TfrmPrincipal.DoExecuteStatement(const Mechanism: IDatabaseMechanism;
  const Command: string; const Params: TParams);
var
  I: Integer;
begin
  mmoSQL.Lines.Add('------------------');
  mmoSQL.Lines.Add(DateTimeToStr(Now) + ' - ' + Command);
  if (Params.Count > 0) then
    for I := 0 to Params.Count - 1 do
      mmoSQL.Lines.Add('   ' + Params.Items[I].Name + ' = ' + Params.Items[I].AsString);
end;

procedure TfrmPrincipal.dsPessoaStateChange(Sender: TObject);
begin
  btnImgAdiciona.Enabled := (tblPessoa.RecordCount > 0);
  btnImgRemover.Enabled := btnImgAdiciona.Enabled;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
//  NewSessionIBX('..\Base\BaseDemo.fdb', 'SYSDBA', 'masterkey', IBDatabase1);
  NewSessionIBX('..\Base\BaseDemo.fdb');
  with (ActiveSession.Mechanism as IDatabaseMechanism) do
  begin
    GenerateSchema(mmoSQL.Lines);
    OnExecuteStatement := Self.DoExecuteStatement;
  end;
  tblPessoa.Session := ActiveSession;
end;

procedure TfrmPrincipal.AfterChanges(Sender: TObject);
begin
  btnApplyChanges.Enabled := False;
  btnCancelChanges.Enabled := False;
end;

procedure TfrmPrincipal.tblPessoaAfterClose(DataSet: TDataSet);
begin
  btnImgAdiciona.Enabled := (tblPessoa.RecordCount > 0);
  btnImgRemover.Enabled := btnImgAdiciona.Enabled;
end;

procedure TfrmPrincipal.tblPessoaAfterPost(DataSet: TDataSet);
begin
  btnApplyChanges.Enabled := True;
  btnCancelChanges.Enabled := True;
end;

procedure TfrmPrincipal.tblPessoaFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  if (cbxTipoPessoa.ItemIndex = 0) then
    Accept:= tblPessoaTipoPessoa.AsString = 'F'
  else
    Accept:= tblPessoaTipoPessoa.AsString = 'J';
end;

procedure TfrmPrincipal.tblPessoaNewRecord(DataSet: TDataSet);
begin
//  tblPessoaDataCadastro.Value:= Now;
end;

end.

