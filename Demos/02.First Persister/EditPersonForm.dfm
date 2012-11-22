object FormEditPerson: TFormEditPerson
  Left = 310
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Edit Person'
  ClientHeight = 370
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object BevelBottom: TBevel
    Left = 0
    Top = 326
    Width = 481
    Height = 3
    Align = alBottom
    Shape = bsBottomLine
  end
  object LabelID: TLabel
    Left = 8
    Top = 8
    Width = 11
    Height = 13
    Caption = 'ID'
  end
  object LabelName: TLabel
    Left = 8
    Top = 50
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object LabelBirthDate: TLabel
    Left = 8
    Top = 92
    Width = 47
    Height = 13
    Caption = 'Birth Date'
  end
  object LabelDocument: TLabel
    Left = 8
    Top = 134
    Width = 49
    Height = 13
    Caption = 'Document'
  end
  object LabelLastUpdate: TLabel
    Left = 384
    Top = 200
    Width = 90
    Height = 17
    Alignment = taRightJustify
    Caption = '20 . 12 . 2006'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Blackletter'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlBottom
  end
  object LabelCredit: TLabel
    Left = 8
    Top = 176
    Width = 27
    Height = 13
    Caption = 'Credit'
  end
  object LabelAddress: TLabel
    Left = 8
    Top = 216
    Width = 38
    Height = 13
    Caption = 'Address'
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 329
    Width = 481
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 7
    object ButtonApply: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 0
    end
    object ButtonNew: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = 'New'
      TabOrder = 1
    end
    object ButtonOK: TButton
      Left = 312
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 2
    end
    object ButtonCancel: TButton
      Left = 400
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
  end
  object EditID: TEdit
    Left = 8
    Top = 24
    Width = 280
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = 'EditID'
  end
  object EditName: TEdit
    Left = 8
    Top = 66
    Width = 280
    Height = 21
    TabOrder = 1
    Text = 'EditName'
  end
  object EditBirthDate: TDateTimePicker
    Left = 8
    Top = 108
    Width = 280
    Height = 21
    Date = 0.504538113433227400
    Time = 0.504538113433227400
    TabOrder = 2
  end
  object EditDocument: TMaskEdit
    Left = 8
    Top = 150
    Width = 277
    Height = 21
    EditMask = '999\.999\.999\-99;0;_'
    MaxLength = 14
    TabOrder = 3
  end
  object EditCredit: TEdit
    Left = 8
    Top = 192
    Width = 281
    Height = 21
    TabOrder = 4
    Text = 'EditCredit'
  end
  object ListBoxAddress: TListBox
    Left = 8
    Top = 232
    Width = 465
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Pitch = fpFixed
    Font.Style = []
    ItemHeight = 11
    ParentFont = False
    TabOrder = 6
  end
  object GroupBoxPicture: TGroupBox
    Left = 296
    Top = 8
    Width = 178
    Height = 188
    Caption = ' Picture '
    TabOrder = 5
    object SpeedButtonLoad: TSpeedButton
      Left = 10
      Top = 154
      Width = 73
      Height = 25
      Caption = 'Load Picture'
    end
    object SpeedButtonClear: TSpeedButton
      Left = 95
      Top = 154
      Width = 73
      Height = 25
      Caption = 'Clear Picture'
    end
    object PanelImage: TPanel
      Left = 10
      Top = 17
      Width = 158
      Height = 132
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      object EditPicture: TImage
        Left = 0
        Top = 0
        Width = 156
        Height = 130
        Align = alClient
        Center = True
        Proportional = True
      end
    end
  end
end
