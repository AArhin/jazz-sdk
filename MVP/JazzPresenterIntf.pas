unit JazzPresenterIntf;

interface

uses JazzModelIntf, JazzViewIntf, JazzValueTypeIntf, JazzInteractorIntf; 

type
  IPresenter = interface(IInterface)
    ['{8CA82EDE-91A9-413E-8685-6F1D00CA33EE}']
    function GetInteractor: IInteractorList;
    function GetModel: IModel;
    function GetView: IView;
    procedure SetModel(const Value: IModel);
    procedure SetView(const Value: IView);

    property Interactor: IInteractorList read GetInteractor;
    property Model: IModel read GetModel write SetModel;
    property View: IView read GetView write SetView;
  end;

  ICompositePresenter = interface(IPresenter)
    ['{8F1C36D0-D9B4-4706-9EEB-BAA2CEA11192}']
    function Add(const PresenterItem: IPresenter): Integer;
    function GetItems(const Index: Integer): IPresenter;
    function IndexOf(const PresenterItem: IPresenter): Integer;
    procedure Clear;
    procedure Delete(const Index: Integer);
    property Items[const Index: Integer]: IPresenter read GetItems; default;
  end;

  IValueTypePresenter = interface(IPresenter)
    ['{7BD8B8AD-FEB7-4898-9812-A6E2797CF3AE}']
  end;

  IMemberPresenter = interface(IValueTypePresenter)
    ['{087F1AF7-81A5-44BF-A4AD-FB74BE557C51}']
  end;

  IObjectPresenter = interface(IMemberPresenter)
    ['{929BAE08-5FD5-4F62-A5B2-C66382B61EFC}']
  end;

  IObjectListPresenter = interface(IMemberPresenter)
    ['{433B09F6-4ABC-4E0C-8726-04A2FCF2488F}']
  end;


implementation

end.
