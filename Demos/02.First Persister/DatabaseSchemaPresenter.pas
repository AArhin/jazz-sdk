unit DatabaseSchemaPresenter;

interface

uses
  JazzInteractor,
  JazzVCLPresenter,
  DatabaseSchemaClassesIntf,
  FirstPersisterPresenterIntf;

type
  TDatabaseSchemaPresenter = class(TFormPresenter, IDatabaseSchemaPresenter)
  private
    FDatabaseSchema: IDatabaseSchemaType;
    procedure DefaultValues;
    function GetDatabaseSchema: IDatabaseSchemaType;
  public
    constructor Create(const AOwner: IInterface); reintroduce; virtual;
  end;

  TDatabaseSchemaInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TQuoteIndentifiersInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TGenerateScriptInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TGenerateSchemaInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

implementation

uses
  JazzDatabaseMechanismIntf,
  JazzDataSetIntf,
  JazzIntfUtils,
  JazzInteractorIntf,
  JazzModel,
  JazzPresenter,
  JazzPresenterIntf,
  JazzSession,
  JazzVCLInteractor,
  JazzVCLView,
  JazzVCLUtils,
  DatabaseSchemaClasses,
  DatabaseSchemaForm,
  SQLFormater,
  Classes,
  Controls,
  Menus,
  SysUtils,
  Windows;

{ TDatabaseSchemaPersister }

constructor TDatabaseSchemaPresenter.Create(const AOwner: IInterface);
begin
  FDatabaseSchema:= TDatabaseSchemaType.Create;
  CreateDatabaseSchemaForm;
  
  inherited Create(AOwner, TObjectModel.Create(FDatabaseSchema),
    TFormView.Create(FormDatabaseSchema));

  Add(TMemoPresenter.Create(Self,
    TMemoModel.Create(FDatabaseSchema.Member['Schema']),
    TEditMemoView.Create(FormDatabaseSchema.MemoSchema)
  ));

  with FormDatabaseSchema do
  begin
    MenuItemClose.OnClick:= Interactor.Add(TFormCloseInteractor).OnExecute;
    MenuItemQuoteIdentifiers.OnClick:= Interactor.Add(TQuoteIndentifiersInteractor).OnExecute;
    MenuItemGenerateScript.OnClick:= Interactor.Add(TGenerateScriptInteractor).OnExecute;
    ToolButtonGenerateScript.OnClick:= Interactor.Add(TGenerateScriptInteractor).OnExecute;
    ToolButtonGenerateSchema.OnClick:= Interactor.Add(TGenerateSchemaInteractor).OnExecute;
  end;

  DefaultValues;
end;

{ TFormPresenter }

procedure TDatabaseSchemaInteractor.Execute(Sender: TObject);
var
  LPresenter: IPresenter;
begin
  LPresenter:= TDatabaseSchemaPresenter.Create(Owner);
  try
    FormDatabaseSchema.ShowModal;
  finally
    FreeAndNil(FormDatabaseSchema);
  end;
end;

procedure TDatabaseSchemaPresenter.DefaultValues;
begin
  Interactor.Get(TQuoteIndentifiersInteractor).Execute(nil);
  Interactor.Get(TGenerateScriptInteractor).Execute(nil);
end;

function TDatabaseSchemaPresenter.GetDatabaseSchema: IDatabaseSchemaType;
begin
  Result:= FDatabaseSchema;
end;

{ TQuoteIndentifiersInteractor }

procedure TQuoteIndentifiersInteractor.Execute(Sender: TObject);
begin
  (ActiveSession.Mechanism as IDatabaseMechanism).DatabaseDriver.
    QuoteIdentifiers:= FormDatabaseSchema.MenuItemQuoteIdentifiers.Checked;
end;

{ TGenerateScriptInteractor }

procedure TGenerateScriptInteractor.Execute(Sender: TObject);
var
  LSchema: TStrings;
begin
  inherited;
  LSchema:= TStringList.Create;
  try
    (ActiveSession.Mechanism as IDatabaseMechanism).GenerateSchema(LSchema);
    TSimpleSQLFormater.Execute(LSchema);
  finally
    (Owner as IDatabaseSchemaPresenter).GetDatabaseSchema.Schema:= LSchema;
    LSchema.Free; 
  end;
end;

{ TGenerateSchemaInteractor }

procedure TGenerateSchemaInteractor.Execute(Sender: TObject);
var
  LScript: string;
  LSQL: string;
  LCommand: IObjectCommand;
begin
  // TODO: Generate diff - sync database
  if (MessageBox(0, 'Generate Database Schema?', 'Warning!', MB_ICONQUESTION or MB_YESNO) = mrYes) then
  begin
    LScript:= (Owner as IDatabaseSchemaPresenter).GetDatabaseSchema.Schema.Text;
    LScript:= StringReplace(LScript, #13#10, ' ',  [rfReplaceAll]);
    LCommand:= (ActiveSession.Mechanism as IDatabaseMechanism).CreateCommand;

    while LScript <> EmptyStr do
    begin
      LSQL:= Copy(LScript, 1, Pos(';', LScript) -1);
      LScript:= Copy(LScript, Pos(';', LScript) + 1, Length(LScript) - Pos(';', LScript));
      LCommand.Execute(LSQL);
    end;
  end;
end;

end.



