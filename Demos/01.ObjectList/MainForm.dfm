object FormMain: TFormMain
  Left = 310
  Top = 168
  Caption = 'ObjectList'
  ClientHeight = 424
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object PanelToolBar: TPanel
    Left = 0
    Top = 0
    Width = 680
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 688
    object GroupBoxInsert: TGroupBox
      Left = 5
      Top = 8
      Width = 170
      Height = 65
      Caption = ' &Insert '
      TabOrder = 0
      object SpinEditInsert: TSpinEdit
        Left = 8
        Top = 25
        Width = 73
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 100
      end
      object ButtonInsert: TButton
        Left = 88
        Top = 24
        Width = 75
        Height = 25
        Action = ActionInsert
        TabOrder = 1
      end
    end
    object GroupBoxDelete: TGroupBox
      Left = 184
      Top = 8
      Width = 170
      Height = 65
      Caption = ' &Delete '
      TabOrder = 1
      object RadioButtonSelected: TRadioButton
        Left = 8
        Top = 18
        Width = 100
        Height = 17
        Caption = '&Selected'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object RadioButtonAll: TRadioButton
        Left = 8
        Top = 38
        Width = 100
        Height = 17
        Caption = '&All'
        TabOrder = 1
      end
      object ButtonDelete: TButton
        Left = 88
        Top = 24
        Width = 75
        Height = 25
        Action = ActionDelete
        TabOrder = 2
      end
    end
    object GroupBox1: TGroupBox
      Left = 360
      Top = 8
      Width = 170
      Height = 65
      Caption = ' Sa&ve to file '
      TabOrder = 2
      object RadioRestore: TRadioButton
        Left = 8
        Top = 38
        Width = 100
        Height = 17
        Caption = '&Restore'
        TabOrder = 2
      end
      object RadioSave: TRadioButton
        Left = 8
        Top = 20
        Width = 57
        Height = 17
        Caption = '&Save'
        TabOrder = 0
      end
      object ButtonFile: TButton
        Left = 88
        Top = 24
        Width = 75
        Height = 25
        Action = ActionSaveRestore
        TabOrder = 1
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 81
    Width = 680
    Height = 343
    Align = alClient
    BevelOuter = bvNone
    Caption = 'PanelPersonList'
    TabOrder = 1
    ExplicitWidth = 688
    ExplicitHeight = 345
    object ListView: TListView
      Left = 0
      Top = 0
      Width = 680
      Height = 324
      Align = alClient
      Columns = <>
      RowSelect = True
      ParentShowHint = False
      ShowWorkAreas = True
      ShowHint = True
      TabOrder = 0
      ExplicitWidth = 688
      ExplicitHeight = 326
    end
    object StatusBarPersonList: TStatusBar
      Left = 0
      Top = 324
      Width = 680
      Height = 19
      Panels = <
        item
          Width = 200
        end
        item
          Width = 200
        end>
      ExplicitTop = 326
      ExplicitWidth = 688
    end
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 96
    object MenuItemFile: TMenuItem
      Caption = 'File'
      object MenuItemClose: TMenuItem
        Caption = 'Close'
        OnClick = MenuItemCloseClick
      end
    end
  end
  object ActionList1: TActionList
    Left = 36
    Top = 96
    object ActionInsert: TAction
      Caption = 'Go'
      OnExecute = ActionInsertExecute
      OnUpdate = ActionInsertUpdate
    end
    object ActionDelete: TAction
      Caption = 'Go'
      OnExecute = ActionDeleteExecute
      OnUpdate = ActionDeleteUpdate
    end
    object ActionSaveRestore: TAction
      Caption = 'Go'
      OnExecute = ActionSaveRestoreExecute
      OnUpdate = ActionSaveRestoreUpdate
    end
  end
end
