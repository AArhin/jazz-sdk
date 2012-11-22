inherited ObjectTableEditor: TObjectTableEditor
  Left = 615
  Top = 241
  Caption = 'ObjectTableEditor.Tabela'
  ClientHeight = 260
  ClientWidth = 135
  ParentFont = False
  OldCreateOrder = False
  ExplicitLeft = 615
  ExplicitTop = 241
  ExplicitWidth = 143
  ExplicitHeight = 294
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter1: TSplitter
    Top = 220
    Width = 135
    ExplicitTop = 246
    ExplicitWidth = 191
  end
  inherited Panel1: TPanel
    Width = 135
    ExplicitWidth = 191
    inherited DBNavigator: TDBNavigator
      Width = 133
      Hints.Strings = ()
      ExplicitWidth = 189
    end
  end
  inherited FieldListBox: TListBox
    Width = 135
    Height = 199
    ExplicitWidth = 191
    ExplicitHeight = 225
  end
  inherited AggListBox: TListBox
    Top = 222
    Width = 135
    ExplicitTop = 248
    ExplicitWidth = 191
  end
end
