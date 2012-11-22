unit PersonManager;

interface

uses Classes, ObjectLinkManager;

type
  IPersonManager = interface(IObjectManager)
    ['{EC604073-732C-478C-BB76-D65A5F9197D6}']
    procedure UpdateSelected;
  end;

  TPersonManager = class(TObjectManager, IPersonManager)
  protected
    procedure CreateColumns; override;
    procedure LinkComponent(const Component: TComponent); override;
    procedure Populate(const Count: Integer); override;
    procedure UpdateSelected;
  end;

implementation

uses BOPerson, BOPersonIntf, RandomData, JazzUtils, JazzTypeInfo, JazzValueType,
  ComCtrls, ViewEditores, SysUtils;

procedure TPersonManager.CreateColumns;
begin
  with ListViewObserver do
  begin
    Clear;
    if CreateIDColumns then
      Add('ID', 'Identification', 300);
    Add('Name', 'Name', 200);
    Add('Document', 'Document', 120);

    if CreateDebugColumns then
    begin
      Add('Loaded', 'Loaded', DebugColumnWidth);
      Add('Persisted', 'Persisted', DebugColumnWidth);
      Add('Modified', 'Modified', DebugColumnWidth);
      Add('Deleting', 'Deleting', DebugColumnWidth);
      Add('Deleted', 'Deleted', DebugColumnWidth);
    end;
  end;
end;

procedure TPersonManager.LinkComponent(const Component: TComponent);
begin
  inherited LinkComponent(Component);

  if Component is TListView then
    CreateColumns;
end;

procedure TPersonManager.Populate(const Count: Integer);
var
  I: Integer;
  LPerson: IPerson;
begin
  ObjectList.BeginUpdate;
  try
    for I:= 1 to Count do
    begin
      LPerson:= ObjectList.Add as IPerson;

//      LPerson.ID:= GenerateGUID;
      LPerson.Name:= RandomPersonName;
      LPerson.Document:= RandomNumber(11);
    end;
  finally
    ObjectList.EndUpdate;
  end;
end;

procedure TPersonManager.UpdateSelected;
var
  LPerson: IPerson;
begin
  LPerson:= ListViewObserver.Selection as IPerson;

  if LPerson <> nil then
  begin
    LPerson.Name:= RandomPersonName;
    LPerson.Document:= RandomNumber(11);
    LPerson.BirthDate:= Now;
  end;
end;

end.
