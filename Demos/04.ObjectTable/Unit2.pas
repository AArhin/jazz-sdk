unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JazzDatabaseMechanismIntf, DB, StdCtrls, ExtCtrls, DBCtrls, Grids,
  DBGrids, ExtDlgs, dbclient, JazzBaseTable, JazzObjectTable;

type
  TForm2 = class(TForm)
    btnAdd: TButton;
    btnApply: TButton;
    btnCancel: TButton;
    btnClear: TButton;
    btnDel: TButton;
    btnLocalizar: TButton;
    Button2: TButton;
    dbgSistema: TDBGrid;
    DBImage: TDBImage;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    dsJazz: TDataSource;
    ledFilter: TLabeledEdit;
    ledLocalizar: TLabeledEdit;
    mmoSQL: TMemo;
    ObjectTable: TObjectTable;
    OpenPictureDialog: TOpenPictureDialog;
    ObjectTableId: TIntegerField;
    ObjectTableDescricao: TStringField;
    ObjectTableComentario: TMemoField;
    ObjectTableFoto: TGraphicField;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnLocalizarClick(Sender: TObject);
  private
    { Private declarations }
    procedure ExecuteStatement(const Mechanism: IDatabaseMechanism; const Command: string; const Params: TParams = nil);
    procedure StartSession;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  JazzValueTypeIntf, JazzValueType, JazzSessionIntf,
  JazzSession, JazzTypeInfo, JazzMappingIntf, JazzIBXMechanism, JazzPersister,
  IBDatabase, JazzCriteriaIntf;

{$R *.dfm}

procedure TForm2.btnAddClick(Sender: TObject);
begin
  if not (ObjectTable.State in dsEditModes) then
    ObjectTable.Edit;
  try
    if OpenPictureDialog.Execute then
      ObjectTableFoto.LoadFromFile(OpenPictureDialog.FileName);
  finally
    ObjectTable.Post;
  end;
end;

procedure TForm2.btnApplyClick(Sender: TObject);
begin
  ObjectTable.ApplyChanges;
end;

procedure TForm2.btnCancelClick(Sender: TObject);
begin
  ObjectTable.CancelChanges;
end;

procedure TForm2.btnClearClick(Sender: TObject);
begin
  mmoSQL.Clear;
end;

procedure TForm2.btnDelClick(Sender: TObject);
begin
  if not (ObjectTable.State in dsEditModes) then
    ObjectTable.Edit;
  try
    ObjectTableFoto.Clear;
  finally
    ObjectTable.Post;
  end;
end;

procedure TForm2.btnLocalizarClick(Sender: TObject);
begin
  ObjectTable.Locate('DESCRICAO', ledLocalizar.Text, []);
  ObjectTable.FindFirst
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  ObjectTable.ClearCriteria;
  if ledFilter.Text <> EmptyStr then
    ObjectTable.Criteria.Add(ctLike, 'descricao', [ledFilter.Text + '%']);
  ObjectTable.Active := not ObjectTable.Active;
end;

procedure TForm2.ExecuteStatement(const Mechanism: IDatabaseMechanism;
  const Command: string; const Params: TParams);
var
  I: Integer;
begin
  mmoSQL.Lines.Add(' ------------------- ');
  mmoSQL.Lines.Add(DateTimeToStr(Now) + ' - ' + Command);
  if (Params.Count > 0) then
    for I := 0 to Params.Count - 1 do
      mmoSQL.Lines.Add('   ' + Params.Items[I].Name + ' = ' + Params.Items[I].AsString);
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ActiveSession.Mechanism.Connected := False;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  // call start session
  StartSession;
end;

procedure TForm2.StartSession;
begin
  NewSession(TIBXMechanism);
  
  with (ActiveSession.Mechanism as IDatabaseMechanism) do
  begin
    TIBDatabase(Connection).SQLDialect := 3;
    TIBDatabase(Connection).DatabaseName := ExtractFilePath(Application.ExeName) + 'Base\CAP.fdb';
    TIBDatabase(Connection).Params.Values['user_name'] := 'SYSDBA';
    TIBDatabase(Connection).Params.Values['password'] := 'masterkey';
    Connected := True;
    GenerateSchema(mmoSQL.Lines);
    OnExecuteStatement := ExecuteStatement;
  end;
  
  ObjectTable.Session := ActiveSession;
end;

end.
