object Form2: TForm2
  Left = 467
  Top = 186
  Caption = 'Form2'
  ClientHeight = 503
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dbgSistema: TDBGrid
    Left = 8
    Top = 79
    Width = 482
    Height = 186
    DataSource = dsJazz
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button2: TButton
    Left = 207
    Top = 17
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = Button2Click
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 48
    Width = 240
    Height = 25
    DataSource = dsJazz
    Kind = dbnHorizontal
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 254
    Top = 48
    Width = 115
    Height = 25
    Caption = 'ApplyChanges'
    TabOrder = 3
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 375
    Top = 48
    Width = 115
    Height = 25
    Caption = 'CancelChanges'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object mmoSQL: TMemo
    Left = 8
    Top = 376
    Width = 563
    Height = 121
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object ledFilter: TLabeledEdit
    Left = 8
    Top = 21
    Width = 193
    Height = 21
    EditLabel.Width = 132
    EditLabel.Height = 13
    EditLabel.Caption = 'Filtrar por nome (inicia com)'
    TabOrder = 1
  end
  object DBMemo1: TDBMemo
    Left = 8
    Top = 271
    Width = 482
    Height = 99
    DataField = 'Comentario'
    DataSource = dsJazz
    TabOrder = 7
  end
  object DBImage: TDBImage
    Left = 496
    Top = 79
    Width = 154
    Height = 186
    DataField = 'Foto'
    DataSource = dsJazz
    Stretch = True
    TabOrder = 8
  end
  object btnAdd: TButton
    Left = 496
    Top = 272
    Width = 75
    Height = 25
    Caption = 'btnAdd'
    TabOrder = 9
    OnClick = btnAddClick
  end
  object btnDel: TButton
    Left = 575
    Top = 271
    Width = 75
    Height = 25
    Caption = 'btnDel'
    TabOrder = 10
    OnClick = btnDelClick
  end
  object btnClear: TButton
    Left = 577
    Top = 472
    Width = 75
    Height = 25
    Caption = 'btnClear'
    TabOrder = 11
    OnClick = btnClearClick
  end
  object btnLocalizar: TButton
    Left = 487
    Top = 17
    Width = 75
    Height = 25
    Caption = 'Localizar'
    TabOrder = 12
    OnClick = btnLocalizarClick
  end
  object ledLocalizar: TLabeledEdit
    Left = 289
    Top = 22
    Width = 193
    Height = 21
    EditLabel.Width = 41
    EditLabel.Height = 13
    EditLabel.Caption = 'Localizar'
    TabOrder = 13
  end
  object dsJazz: TDataSource
    DataSet = ObjectTable
    Left = 472
    Top = 184
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.w' +
      'mf|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg' +
      '|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*' +
      '.emf)|*.emf|Metafiles (*.wmf)|*.wmf'
    Left = 568
    Top = 312
  end
  object ObjectTable: TObjectTable
    ObjectClassName = 'TCAP_Sistema'
    Left = 256
    Top = 160
    object ObjectTableId: TIntegerField
      FieldName = 'Id'
    end
    object ObjectTableDescricao: TStringField
      FieldName = 'Descricao'
      Size = 40
    end
    object ObjectTableComentario: TMemoField
      FieldName = 'Comentario'
      BlobType = ftMemo
      Size = 255
    end
    object ObjectTableFoto: TGraphicField
      FieldName = 'Foto'
      BlobType = ftGraphic
    end
  end
end
