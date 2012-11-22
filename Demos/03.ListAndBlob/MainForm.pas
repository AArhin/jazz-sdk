unit MainForm;

{$WARN UNIT_DEPRECATED OFF}

interface

uses
  Classes,
  ComCtrls,
  Controls,
  DB,
  DBTables,
  Dialogs,
  ExtCtrls,
  ExtDlgs,
  Forms,
  Graphics,
  IBDatabase,
  JazzDatabaseMechanismIntf,
  JazzValueTypeIntf,
  JazzVCLPresenter,
  JazzVCLPresenterIntf,
  JazzVCLView,
  JazzVCLViewIntf,
  Messages,
  StdCtrls,
  SysUtils,
  ThemedDBGrid,
  Variants,
  WideStrings,
  Windows,
  XPMan;

type
  TForm1 = class(TForm)
    ButtonAdd: TButton;
    ButtonAddFoto: TButton;
    ButtonDeleteAll: TButton;
    ButtonImporty: TButton;
    ButtonLoad: TButton;
    ButtonLoadList: TButton;
    ButtonRemoveFoto: TButton;
    CheckBoxAjustarFoto: TCheckBox;
    EditAdd: TEdit;
    EditImage: TImage;
    IBDatabase1: TIBDatabase;
    ListViewEnderecos: TListView;
    ListViewPessoa: TListView;
    MemoList: TMemo;
    MemoSQL: TMemo;
    PageControl: TPageControl;
    PanelFoto: TPanel;
    PanelList: TPanel;
    PanelTools: TPanel;
    PictureDialog: TOpenPictureDialog;
    Splitter: TSplitter;
    TabSheetListView: TTabSheet;
    TabSheetMemoList: TTabSheet;
    TabSheetSQL: TTabSheet;
    XPManifest1: TXPManifest;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonAddFotoClick(Sender: TObject);
    procedure ButtonDeleteAllClick(Sender: TObject);
    procedure ButtonImportyClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonLoadListClick(Sender: TObject);
    procedure ButtonRemoveFotoClick(Sender: TObject);
    procedure CheckBoxAjustarFotoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FPessoaList: IObjectListType;
    FPessoaListPresenter: IListViewPresenter;
    FEnderecoListPresenter: IListViewPresenter;
    FImagePresenter: ISimpleControlPresenter;
    procedure ExecuteStatement(const Mechanism: IDatabaseMechanism; const Command: string; const Params: TParams = nil);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure StartSession;
    procedure Populate(Count: Integer = 100);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ClassesImpl,
  ClassesIntf,
  JazzPersister,
  JazzSession,
  JazzIBXMechanism,
  JazzDBXFirebirdMechanism,
  JazzMasterDetailIntf,
  JazzModel,
  JazzVCLModel,
  JazzSessionIntf,
  RandomData,
  JazzMappingIntf;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  StartSession;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IBDatabase1.Close;
end;

procedure TForm1.ButtonAddClick(Sender: TObject);
begin
  FPessoaList.BeginUpdate;
  Populate(StrToInt(EditAdd.Text));
  FPessoaList.EndUpdate;
end;

procedure TForm1.ButtonAddFotoClick(Sender: TObject);
begin
  if PictureDialog.Execute then
  begin
    EditImage.Picture.LoadFromFile(PictureDialog.FileName);
    ActiveSession.Save(FPessoaList);
  end;
end;

procedure TForm1.ButtonDeleteAllClick(Sender: TObject);
var
  LCount: Integer;
begin
  with ActiveSession do
  begin
    if FPessoaList.Count = 0 then Load(FPessoaList);
    LCount:= FPessoaList.Count;
    (Mechanism as IDatabaseMechanism).DeleteObjectList(FPessoaList);
  end;
  MessageDlg(Format('%d objetos excluidos', [LCount]), mtInformation, [mbOK], 0);
end;

procedure TForm1.ButtonImportyClick(Sender: TObject);
var
  LMapping: IMapping;
  I, J: Integer;
begin
  LMapping:= (ActiveSession.Mechanism as IDatabaseMechanism).DatabaseDriver.GetDatabaseSchema;
  MemoList.Clear;
  for I := 0 to LMapping.Count - 1 do
  begin
    MemoList.Lines.Add(LMapping[I].ObjectClassName);

    for J := 0 to LMapping[I].Count - 1 do    
      MemoList.Lines.Add('    ' + (LMapping[I].Items[J] as IMemberMeta).MemberName);
  end;
end;

procedure TForm1.ButtonLoadClick(Sender: TObject);
var
  I: Integer;
  LPessoa: IPessoa;
  LOutraPessoa: IPessoa;
begin
  MemoList.Lines.Clear;
  MemoList.Lines.BeginUpdate;
  try
    for I:= 0 to FPessoaList.Count - 1 do
    begin
      LPessoa:= FPessoaList[I] as IPessoa;
      MemoList.Lines.Add(
        IntToStr(I) + ', ' +
        LPessoa.ID + ', ' +
        LPessoa.Nome + ', ' +
        DateToStr(LPessoa.DataNascimento)
        );
    end;
  finally
    MemoList.Lines.EndUpdate;
  end;

  LPessoa:= FPessoaList[0] as IPessoa;
  LOutraPessoa:= LPessoa.Clone as IPessoa;;
end;

procedure TForm1.ButtonLoadListClick(Sender: TObject);
begin
  FPessoaList.Clear;
  ActiveSession.Cache.Clear;
  ActiveSession.Load(FPessoaList);
end;

procedure TForm1.ButtonRemoveFotoClick(Sender: TObject);
begin
  if (MessageDlg('Remover Imagem?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    EditImage.Picture.Graphic:= nil;
    ActiveSession.Save(FPessoaList);
  end;
end;

procedure TForm1.CheckBoxAjustarFotoClick(Sender: TObject);
begin
  EditImage.Stretch:= CheckBoxAjustarFoto.Checked;
end;

procedure TForm1.ExecuteStatement(const Mechanism: IDatabaseMechanism; const
  Command: string; const Params: TParams);
begin
  MemoSQL.Lines.Add(DateTimeToStr(Now) + ' - ' + Command);
end;

procedure TForm1.Populate(Count: Integer);
var
  I, J: Integer;
  LCount: Integer;
  LPessoa: IPessoa;
  LEndereco: IEndereco;
begin
  for I:= 1 to Count do
  begin
    LPessoa:= FPessoaList.Add as IPessoa;
    //LPessoa.ID:= IntToStr(I);
    LPessoa.Nome:= RandomName;
    LPessoa.DataNascimento:= Now;

    LCount:= StrToInt(RandomNumber(1));
    for J:= 1 to LCount do
    begin
      LEndereco:= LPessoa.EnderecoList.Add as IEndereco;

      with LEndereco do
      begin
        Rua:= RandomStreet;
        Numero:= StrToInt(RandomNumber(4));
        Cidade:= RandomCity;
        UF:= RandomState;
      end;
    end;
  end;

  ActiveSession.Save(FPessoaList);
end;

procedure TForm1.StartSession;
begin
  NewSession(TIBXMechanism);
  with (ActiveSession.Mechanism as IDatabaseMechanism) do
  begin
    GenerateSchema(MemoSQL.Lines);
    Connection:= IBDatabase1;
    IBDatabase1.Connected:= True;
    OnExecuteStatement:= ExecuteStatement;
  end;

  // Lista/Presenter Mestre
  FPessoaList:= TPessoa.NewList;
  FPessoaListPresenter:= TListViewPresenter.Create(FPessoaList, ListViewPessoa);
  with FPessoaListPresenter, ListViewPessoa do
  begin
    Add(Columns[0], 'ID'); // do not localize
    Add(Columns[1], 'Nome'); // do not localize
    Add(Columns[2], 'DataNascimento'); // do not localize
  end;

  // Presenter Detalhe - Lista = nil, definida no OnSelect do Mestre
  FEnderecoListPresenter:= TListViewPresenter.Create(nil, ListViewEnderecos);
  with FEnderecoListPresenter, ListViewEnderecos do
  begin
    Add(Columns[0], 'ID'); // do not localize
    Add(Columns[1], 'Rua'); // do not localize
    Add(Columns[2], 'Numero'); // do not localize
    Add(Columns[3], 'Bairro'); // do not localize
    Add(Columns[4], 'Cidade'); // do not localize
    Add(Columns[5], 'UF'); // do not localize
    Add(Columns[6], 'IdOwner'); // do not localize
  end;

  (FEnderecoListPresenter as IDetail).SetMaster(FPessoaListPresenter,
    'EnderecoList'); // do not localize

  FImagePresenter:= TSimpleControlPresenter.Create(
    Self,
    TBlobModel.Create(nil),
    TImageView.Create(EditImage)
  );

  (FImagePresenter as IDetail).SetMaster(FPessoaListPresenter, 'Foto'); // do not localize
end;

end.

