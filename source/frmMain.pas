unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.Dialogs, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  lib.firebase.rest, DBXJSON, System.JSON, Data.DBXJSONCommon, lib.document, generics.collections,
  FMX.Layouts, FMX.ListBox;

type
  Tmain = class(TForm)
    ActionList1: TActionList;
    PreviousTabAction1: TPreviousTabAction;
    TitleAction: TControlAction;
    NextTabAction1: TNextTabAction;
    TopToolBar: TToolBar;
    btnBack: TSpeedButton;
    ToolBarLabel: TLabel;
    btnNext: TSpeedButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    BottomToolBar: TToolBar;
    Refresh: TAction;
    SpeedButton1: TSpeedButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure TitleActionUpdate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure RefreshExecute(Sender: TObject);
  private
    function LoadDocuments(jsonString: string): TList<IDocument>;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  main: Tmain;

implementation

uses
  StrUtils;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.iPhone4in.fmx IOS}

procedure Tmain.TitleActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    if TabControl1.ActiveTab <> nil then
      TCustomAction(Sender).Text := TabControl1.ActiveTab.Text
    else
      TCustomAction(Sender).Text := '';
  end;
end;

procedure Tmain.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.First(TTabTransition.None);
end;

procedure Tmain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkHardwareBack) and (TabControl1.TabIndex <> 0) then
  begin
    TabControl1.First;
    Key := 0;
  end;
end;

procedure Tmain.RefreshExecute(Sender: TObject);
var
  firebase : string;
  List : TList<IDocument>;
  i: Integer;
  listitem : TListViewItem;
  ListBoxItem : TListBoxItem;
begin
  //Load files from the cloud
  firebase := TFirebaseRest.New.GetCollection;
  List := LoadDocuments(firebase);

  ListBox1.BeginUpdate;
  for i := 0 to list.Count-1 do
  begin
    ListBoxItem := TListBoxItem.Create(ListBox1);
    ListBoxItem.Text := list[i].FileName;
    ListBoxItem.Data :=  TDocument(list[i]);
    ListBoxItem.ItemData.Accessory := TListBoxItemData.TAccessory(2);
    ListBox1.AddObject(ListBoxItem);
  end;
  ListBox1.EndUpdate;
end;

function Tmain.LoadDocuments(jsonString : string) : TList<IDocument>;
var
  s : string;
  i : integer;
  j, k : integer;
  doc : string;
  name : string;
  list : TList<IDocument>;
begin
  list := TList<IDocument>.Create;
  s := jsonString;
  i := 1;
  while i > 0 do
  begin
    i := AnsiPos('array', s);
    j := AnsiPos('document', s);
    if ((i > 0) and (j>0)) then
    begin
      doc := copy(s, i, j-i);
      doc := doc.Replace('array":', '');
      doc := doc.Replace(',"', '');
      name := AnsiRightStr(s, length(s)-j+1);
      k := AnsiPos('}', name);
      name := copy(name, 0, k);
      name := name.Replace('document":"', '');
      name := name.Replace('"}', '');
      s := AnsiRightStr(s, length(s)-(j+k));
      list.Add(TDocument.New(name, doc));
    end;
  end;
  result := list;
end;

end.
