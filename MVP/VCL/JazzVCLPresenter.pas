unit JazzVCLPresenter;

interface

uses
  JazzPresenter,
  JazzPresenterIntf,
  JazzValueTypeIntf,
  JazzVCLModel,
  JazzVCLView,
  JazzVCLViewIntf,
  JazzVCLPresenterIntf;

type
  TSimpleControlPresenter = class(TMemberPresenter, ISimpleControlPresenter);
  TObjectControlPresenter = class(TObjectPresenter, IObjectControlPresenter);
  TListControlPresenter = class(TObjectListPresenter, IListControlPresenter);

  TListItemPresenter = class(TSimpleControlPresenter)
  end;

  TListViewPresenter = class(TListControlPresenter, IListViewPresenter)
  private
    FListView: IListViewView;
  protected
    function Add(const Column: TObject; const MemberName: string): IValueTypePresenter; overload;
  public
    constructor Create(const AOwner: IInterface; const ObjectList: IObjectListType; const Control: TObject); reintroduce; overload; virtual;
    constructor Create(const ObjectList: IObjectListType; const Control: TObject); reintroduce; overload; virtual;
    destructor Destroy; override;
  end;

  TTreeViewPresenter = class(TListControlPresenter, ITreeViewPresenter)
  private
    FTreeView: ITreeViewView;
  public
    constructor Create(const AOwner: IInterface; const ObjectList: IObjectListType; const Control: TObject); reintroduce; overload; virtual;
    constructor Create(const ObjectList: IObjectListType; const Control: TObject); reintroduce; overload; virtual;
    destructor Destroy; override;
  end;
  
  TFormPresenter = class(TObjectControlPresenter)
  end;
  
implementation

uses
  SysUtils;

{ TListViewPresenter }

function TListViewPresenter.Add(const Column: TObject; const MemberName: string): IValueTypePresenter;
begin
  Result:= TListItemPresenter.Create(Self, TListItemModel.Create(nil),
    FListView.Add(Column, MemberName));

  inherited Add(Result);
end;

constructor TListViewPresenter.Create(const AOwner: IInterface;
  const ObjectList: IObjectListType; const Control: TObject);
begin
  FListView:= TListViewView.Create(Control);
  inherited Create(AOwner, TListViewModel.Create(ObjectList), FListView);
end;

constructor TListViewPresenter.Create(const ObjectList: IObjectListType;
  const Control: TObject);
begin
  Create(nil, ObjectList, Control);
end;

destructor TListViewPresenter.Destroy;
begin
  FListView:= nil; 
  inherited;
end;

{ TTreeViewPresenter }

constructor TTreeViewPresenter.Create(const AOwner: IInterface;
  const ObjectList: IObjectListType; const Control: TObject);
begin
  FTreeView:= TTreeViewView.Create(Control);
  inherited Create(AOwner, TTreeViewModel.Create(ObjectList), FTreeView);
end;

constructor TTreeViewPresenter.Create(const ObjectList: IObjectListType;
  const Control: TObject);
begin
  Create(nil, ObjectList, Control);
end;

destructor TTreeViewPresenter.Destroy;
begin
  FTreeView.Clear;
  inherited;
end;

end.



