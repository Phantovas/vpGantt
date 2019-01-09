unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids, vpGantt,
  gsGanttCalendar, eventlog;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ChBStyle: TCheckBox;
    ChBFlat: TCheckBox;
    EL: TEventLog;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure ChBFlatChange(Sender: TObject);
    procedure ChBStyleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure LogBoundsRect(Control: TControl);
    procedure LogClientRect(Control: TControl);
  public
    { public declarations }
    procedure Debug(AMessage: string; AShowMessage: boolean = false);
    procedure ShowParam(const AMsg: string);
  end;

var
  Form1: TForm1;
  GanttDiagram: TvpGantt;
  GC: TgsGantt;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Debug('TForm1.Button1Click');
  LogBoundsRect(GanttDiagram);
  LogClientRect(GanttDiagram);
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
    GC.Free;
    GanttDiagram.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i, j: integer;
  LInterval: TInterval;
  GInterval: TvpInterval;
begin
  EL.Active := True;

  GanttDiagram := TvpGantt.Create(Self);
  GanttDiagram.Parent := Self;
  GanttDiagram.Left := 10;
  GanttDiagram.Top := 310;
  GanttDiagram.Width := 500;
  GanttDiagram.Height := 300;
  GanttDiagram.ScrollBars := ssBoth;
  Debug(Format('TForm1.FormShow RowHeight %d', [GanttDiagram.RowHeight]));
  for i:=0 to 25 do
    begin
      GInterval := TvpInterval.Create(GanttDiagram);
      GInterval.Name := 'aaassadsfsdfgdsfgdsgsdfgdfsdfsdffffffffffffffffffffffffffffffffffffffffffffff' + IntToStr(i) ;
      GInterval.StartDate := Now + i/60/60*i + 1;
      //GInterval.FinishDate := Now + i/60/60*i + 2;
      //GInterval.Visible := True;
      GanttDiagram.AddInterval(GInterval);
    end;
  GanttDiagram.TaskTitleCaption := 'Проекты';
  GanttDiagram.First;
  GanttDiagram.SetFocus;

  GC :=  TgsGantt.Create(Self);
  GC.Parent := Self;
  GC.Left := 0;
  GC.Top := 0;
  GC.Width:= 500;
  GC.Height:= 200;
  GC.Tree.Font.Color := clRed;
  GC.Calendar.StartDate:= Now;
  GC.Calendar.MinorScale := tsMinute;
  GC.Calendar.MajorScale:= tsHour;
  for i:=0 to 5 do
    begin
      Linterval := TInterval.Create(GC);
      //LInterval.Name := 'aa' + IntToStr(i) ;
      LInterval.StartDate := Now + i/60/60*i + 1;
      LInterval.FinishDate := Now + i/60/60*i + 2;
      LInterval.Visible := True;
      GC.AddInterval(LInterval);
      GC.Tree.Cells[0,i + 1] := IntToStr(i);
      for j:=0 to 3 do
        begin
          Linterval := TInterval.Create(GC);
          //LInterval.Name := 'bbbbbb ' + IntToStr(j);
          LInterval.StartDate := Now + i/60/60*i + j/60/60;
          LInterval.FinishDate := Now + i/60/60*i + j/60/60  + 1;
          GC.Interval[i].AddInterval(LInterval);
        end;
    end;
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

end.

