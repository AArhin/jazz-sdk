unit EditPersonPresenter;

interface

uses
  JazzInteractor,
  JazzValueTypeIntf,
  JazzVCLPresenter,
  EditPersonPresenterIntf;

type
  TEditPersonPresenter = class(TFormPresenter, IEditPersonPresenter)
  public
    constructor Create(const AOwner: IInterface; const ObjectValue: IObjectType); reintroduce;
  end;

  TEditPersonInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TNewPersonInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

implementation

uses
  JazzModel,
  JazzPresenterIntf,
  JazzVCLView,
  BOPersonWithAddressIntf,
  FirstPersisterPresenterIntf,
  EditPersonForm,
  Controls,
  SysUtils;

{ TEditPersonPresenter }

constructor TEditPersonPresenter.Create(const AOwner: IInterface;
  const ObjectValue: IObjectType);
begin
  CreateEditPersonForm;
  inherited Create(AOwner,
    TObjectModel.Create(ObjectValue),
    TFormView.Create(FormEditPerson));

  Add(TSimpleControlPresenter.Create(Self,
    TStringModel.Create(ObjectValue.Member['ID']),
    TEditView.Create(FormEditPerson.EditID)));
  
  Add(TSimpleControlPresenter.Create(Self,
    TStringModel.Create(ObjectValue.Member['Name']),
    TEditView.Create(FormEditPerson.EditName)));

  Add(TSimpleControlPresenter.Create(Self,
    TStringModel.Create(ObjectValue.Member['BirthDate']),
    TDateTimePickerView.Create(FormEditPerson.EditBirthDate)));

  Add(TSimpleControlPresenter.Create(Self,
    TStringModel.Create(ObjectValue.Member['Document']),
    TMaskEditView.Create(FormEditPerson.EditDocument)));

  Add(TSimpleControlPresenter.Create(Self,
    TStringModel.Create(ObjectValue.Member['Credit']),
    TEditView.Create(FormEditPerson.EditCredit)));

  Add(TSimpleControlPresenter.Create(Self,
    TStringModel.Create(ObjectValue.Member['LastUpdate']),
    TLabelView.Create(FormEditPerson.LabelLastUpdate)));
end;

{ TFormPresenter }

procedure TEditPersonInteractor.Execute(Sender: TObject);
var
  LPresenter: IPresenter;
  LActivePerson: IPersonWithAddress;
  LEditPerson: IPersonWithAddress;
begin
  LActivePerson:= Model.Selection.Active as IPersonWithAddress;
  LEditPerson:= (Model.ObjectValue as IObjectListType).New as IPersonWithAddress;
  LEditPerson.Assign(LActivePerson);
  LPresenter:= TEditPersonPresenter.Create(Owner, LEditPerson);
  try
    if FormEditPerson.ShowModal = mrOK then LActivePerson.Assign(LEditPerson);
  finally
    FreeAndNil(FormEditPerson);
  end;
end;

{ TNewPersonInteractor }

procedure TNewPersonInteractor.Execute(Sender: TObject);
var
  LPresenter: IPresenter;
  LPerson: IPersonWithAddress;
begin
  LPerson:= (Model.ObjectValue as IObjectListType).New as IPersonWithAddress;
  LPresenter:= TEditPersonPresenter.Create(Owner, LPerson);
  try
    if FormEditPerson.ShowModal = mrOK then LPerson.Assign(LPerson);
  finally
    FreeAndNil(FormEditPerson);
  end;
end;

end.
