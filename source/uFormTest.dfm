object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 657
  ClientWidth = 1546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 640
    Top = 49
    Width = 94
    Height = 25
    Caption = 'Load Firebase'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 400
    Top = 111
    Width = 334
    Height = 522
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Edit1: TEdit
    Left = 45
    Top = 53
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 172
    Top = 53
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
  end
  object Button2: TButton
    Left = 299
    Top = 49
    Width = 110
    Height = 25
    Caption = 'Add To Firebase'
    TabOrder = 4
    OnClick = Button2Click
  end
  object AddFile: TButton
    Left = 45
    Top = 80
    Width = 110
    Height = 25
    Caption = 'Add File To Firebase'
    TabOrder = 5
    OnClick = AddFileClick
  end
  object Button3: TButton
    Left = 559
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = Button3Click
  end
  object GetFile: TButton
    Left = 161
    Top = 80
    Width = 120
    Height = 25
    Caption = 'Get File Firebase'
    TabOrder = 7
    OnClick = GetFileClick
  end
  object Memo2: TMemo
    Left = 752
    Top = 111
    Width = 689
    Height = 522
    TabOrder = 8
  end
  object Button4: TButton
    Left = 559
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 9
    OnClick = Button4Click
  end
  object ListBox1: TListBox
    Left = 45
    Top = 111
    Width = 340
    Height = 522
    ItemHeight = 13
    TabOrder = 10
  end
  object OpenDialog1: TOpenDialog
    Left = 200
  end
end
