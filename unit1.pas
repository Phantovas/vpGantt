unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids, vpGantt,
  eventlog, dateutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ChBStyle: TCheckBox;
    ChBFlat: TCheckBox;
    EL: TEventLog;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ChBFlatChange(Sender: TObject);
    procedure ChBStyleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  private
    { private declarations }
    procedure LogBoundsRect(Control: TControl);
    procedure LogClientRect(Control: TControl);
    procedure MDown(Sender: Tobject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure MMove(Sender: Tobject; Shift:TShiftState; X,Y:Integer);
    procedure MUp(Sender: Tobject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure Click(Sender: TObject);
    procedure DblClick(Sender: TObject);
  public
    { public declarations }
    procedure Debug(AMessage: string; AShowMessage: boolean = false);
    procedure ShowParam(const AMsg: string);
  end;

var
  Form1: TForm1;
  GanttDiagram: TvpGantt;
  current: integer;
  //GC: TgsGantt;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Debug('TForm1.Button1Click');
  Panel4.Canvas.Brush.Color := clGreen;
  Panel4.Canvas.pen.Color := clBlack;
  Panel4.Canvas.Rectangle(0,0,25,25);
  Panel4.Canvas.Brush.Color := clRed;
  PAnel4.Canvas.FloodFill(1,1, clGreen, fsSurface);
  //Panel4.Canvas.Rectangle(0,0, 10,10);
  //Panel4.Canvas.Rectangle(10,10, Panel4.ClientWidth, Panel4.ClientHeight);
  Memo1.Lines.Add('Panel4.ClientWidth ' + IntToStr(Panel4.ClientWidth));
  Memo1.Lines.Add('Panel4.ClientREct.Left ' + IntToStr(Panel4.ClientREct.LEft));
  Memo1.Lines.Add('Panel4.ClientREct.Right ' + IntToStr(Panel4.ClientREct.Right));
  Memo1.Lines.Add('Panel4.ClientHeight ' + IntToStr(Panel4.ClientHeight));
  Memo1.Lines.Add('Panel4.ClientREct.Top ' + IntToStr(Panel4.ClientREct.Top));
  Memo1.Lines.Add('Panel4.ClientREct.Bottom ' + IntToStr(Panel4.ClientREct.Bottom));
  Memo1.Lines.Add('Panel4.ClientREct.Width ' + IntToStr(Panel4.ClientREct.Width));
  Memo1.Lines.Add('Panel4.ClientREct.HEight ' + IntToStr(Panel4.ClientREct.Height));

  StringGrid1.GridLineWidth := StringGrid1.GridLineWidth +1;
  Memo1.Lines.Add('StringGrid1.GridLineWidth ' + IntToStr(StringGrid1.GridLineWidth));

  GanttDiagram.ScrollToCurTime;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: integer;
  //LInterval: TInterval;
  GInterval: TvpInterval;
begin
  GanttDiagram.BeginUpdate;
  for i:=current to current+10 do
    begin
      GInterval := TvpInterval.Create(GanttDiagram);
      GInterval.Name := 'Наряд №' + IntToStr(i) + ' от ' + DateToStr(Now-10+i);
      GInterval.StartDate := Now - 4 + i;
      GInterval.Duration := 10;
      if i = 4 then
        GInterval.FinishDate := GInterval.StartDate + 4;
      if i = 6 then
        GInterval.FinishDate := GInterval.StartDate + 9;
      if i = 8 then
        GInterval.FinishDate := GInterval.StartDate + 10;
      if i = 10 then
        GInterval.FinishDate := GInterval.StartDate + 11;
      //GInterval.Visible := True;
      GanttDiagram.AddInterval(GInterval);
    end;
  GanttDiagram.EndUpdate();
  current := i+1;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  GanttDiagram.Clear;
end;

procedure TForm1.ChBFlatChange(Sender: TObject);
begin
  Debug('TForm1.ChBFlatChange');
  LogBoundsRect(GanttDiagram);
  LogClientRect(GanttDiagram);
end;

procedure TForm1.ChBStyleChange(Sender: TObject);
begin
  Debug('TForm1.ChBStyleChange');
  LogBoundsRect(GanttDiagram);
  LogClientRect(GanttDiagram);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
  finally
    //GC.Free;
    GanttDiagram.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  EL.Active := True;
  current := 0;

  GanttDiagram := TvpGantt.Create(Self);
  GanttDiagram.Parent := Panel3;
  GanttDiagram.Align := alClient;
  GanttDiagram.ScrollBars := ssAutoBoth;
  GanttDiagram.BorderStyle := bsSingle;
  GanttDiagram.TaskTitleCaption := 'Наряды';
  GanttDiagram.MinorScale := vptsHour;
  GanttDiagram.MajorScale := vptsDay;
  GanttDiagram.TitleStyle := tsNative;
  GanttDiagram.GridBorderWidth := 1;
  GanttDiagram.Hint := 'Это стандартный скрипт';
  //Debug(Format('TForm1.FormShow RowHeight %d', [GanttDiagram.RowHeight]));
  GanttDiagram.SetFocus;
  GanttDiagram.OnMouseDown := @MDown;
  GanttDiagram.OnMouseMove := @MMove;
  GanttDiagram.OnMouseUp := @MUp;
  GanttDiagram.OnDblClick := @DblClick;
  //GanttDiagram.OnClick := @Click;
  //StringGrid1.OnClick := @Click;
  StringGrid1.OnDblClick := @DblClick;

  //GC :=  TgsGantt.Create(Self);
  //GC.Parent := Self;
  //GC.Left := 0;
  //GC.Top := 0;
  //GC.Width:= 500;
  //GC.Height:= 200;
  //GC.Tree.Font.Color := clRed;
  //GC.Calendar.StartDate:= Now;
  //GC.Calendar.MinorScale := tsMinute;
  //GC.Calendar.MajorScale:= tsHour;
  //for i:=0 to 5 do
  //  begin
  //    Linterval := TInterval.Create(GC);
  //    //LInterval.Name := 'aa' + IntToStr(i) ;
  //    LInterval.StartDate := Now + i/60/60*i + 1;
  //    LInterval.FinishDate := Now + i/60/60*i + 2;
  //    LInterval.Visible := True;
  //    GC.AddInterval(LInterval);
  //    GC.Tree.Cells[0,i + 1] := IntToStr(i);
  //    for j:=0 to 3 do
  //      begin
  //        Linterval := TInterval.Create(GC);
  //        //LInterval.Name := 'bbbbbb ' + IntToStr(j);
  //        LInterval.StartDate := Now + i/60/60*i + j/60/60;
  //        LInterval.FinishDate := Now + i/60/60*i + j/60/60  + 1;
  //        GC.Interval[i].AddInterval(LInterval);
  //      end;
  //  end;
end;

procedure TForm1.Panel2Click(Sender: TObject);
begin
  ShowMessage('PanelClick');
end;

procedure TForm1.Debug(AMessage: string; AShowMessage: boolean);
begin
  EL.Debug(AMessage);
  if AShowMessage then
    ShowMessage(AMessage);
end;

procedure TForm1.ShowParam(const AMsg: string);
begin
  Label1.Caption := AMsg;
end;

procedure TForm1.LogBoundsRect(Control: TControl);
begin
  Memo1.Lines.Add('BoundsRect.Left ' + IntToStr(Control.BoundsRect.Left));
  Memo1.Lines.Add('BoundsRect.Top ' + IntToStr(Control.BoundsRect.Top));
  Memo1.Lines.Add('BoundsRect.Width ' + IntToStr(Control.BoundsRect.Width));
  Memo1.Lines.Add('BoundsRect.Height ' + IntToStr(Control.BoundsRect.Height));
end;

procedure TForm1.LogClientRect(Control: TControl);
begin
  Memo1.Lines.Add('ClientRect.Left ' + IntToStr(Control.ClientRect.Left));
  Memo1.Lines.Add('ClientRect.Top ' + IntToStr(Control.ClientRect.Top));
  Memo1.Lines.Add('ClientRect.Width ' + IntToStr(Control.ClientRect.Width));
  Memo1.Lines.Add('ClientRect.Height ' + IntToStr(Control.ClientRect.Height));
end;

procedure TForm1.MDown(Sender: Tobject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  Memo1.Lines.Add('OnMouseDown ' + Format('x %d y %d',[X, Y]));
end;

procedure TForm1.MMove(Sender: Tobject; Shift: TShiftState; X, Y: Integer);
begin
  Memo1.Lines.Add('OnMouseMove ' + Format('x %d y %d',[X, Y]));
end;

procedure TForm1.MUp(Sender: Tobject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Memo1.Lines.Add('OnMouseUp ' + Format('x %d y %d',[X, Y]));
end;

procedure TForm1.Click(Sender: TObject);
begin
  ShowMessage('Click');
end;

procedure TForm1.DblClick(Sender: TObject);
var
  curIndex: integer;
begin
  curIndex := GanttDiagram.SelectedIndex;
  Memo1.Lines.Add(IntToStr(curIndex));
end;

end.

