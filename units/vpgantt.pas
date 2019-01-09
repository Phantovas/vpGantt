unit vpGantt;

{$define DbgScroll}

interface

uses
  LCLproc, LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls,
  Graphics, StdCtrls, Forms, ExtCtrls, Math,
  //temp
  Dialogs;

const
  C_DEFAULT_SPLITTER_WIDTH = 2;
  C_DEFAULT_TASKS_WIDTH = 150;
  C_DEFAULT_BORDER_WIDTH = 1;
  C_DEFAULT_ROW_HEIGHT = 20;
  C_VPGANTT_WIDTH = 300;
  C_VPGANTT_HEIGHT = 150;

type
  TTitleStyle = (tsLazarus, tsStandard, tsNative);
  TvpgOption = (vpgDrawFocusSelected, vpgFocusHighlight, vpgRowHighlight);
  TvpGanttOptions = set of TvpgOption;


const
  SCROLL_PAGE_DEFAULT = 100;
  constCellPadding: byte = 3;
  constRubberSpace: byte = 2;
  DefaultGanttOptions = [vpgFocusHighlight];

type
  TvpGantt = class;

  { TvpBaseInterval }

  TvpBaseInterval = class
  private
    FResource: string;
  protected
    FName: string;
    FProject: string;
    FFinishDate: TDateTime;
    FStartDate: TDateTime;
    FPlan: Double;
  public
    property StartDate : TDateTime read FStartDate write FStartDate;
    property DueDate : TDateTime read FFinishDate write FFinishDate;
    property PlanTime : Double read FPlan write FPlan;
    property Name : string read FName write FName;
    property Project : string read FProject write FProject;
    property Resource : string read FResource write FResource;
  end;

  { TvpInterval }

  TvpInterval = class(TvpBaseInterval)
    private
      FvpGantt: TvpGantt;
    protected
    public
      constructor Create(AvpGantt: TvpGantt); virtual;
      destructor Destroy; override;
  end;

  { TvpGanttTasks }

  TvpGanttTasks = class(TCustomControl)
    private
      FvpGantt: TvpGantt;
      //vars
      FTasksWidth: integer;
      FTasksHeight: integer;
      FTitleHeight: integer;
      FXScrollPosition: integer;
      //scrollbars
      FHSbVisible: boolean;
      //methods
      procedure CalcFocusRect(var aRect: TRect);
      function CalcRowRect(aRow: integer): TRect;
      procedure CalcTasksHeight;
      procedure CalcTasksWidth;
      procedure CalcTitleHeight;
      procedure CalcScrollbarsRange;
      function GetTitleRect: TRect;
      procedure VisualChange; virtual;
      procedure UpdateSizes;
      //messages
      procedure WMSize(var Message: TLMSize);
        message LM_SIZE;
    protected
      procedure CreateParams(var Params: TCreateParams); override;
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure DrawAllRows;
      procedure DrawEdges;
      procedure DrawFocusRect(aRow: integer; ARect: TRect); virtual;
      procedure DrawRow(aRow: integer);
      procedure DrawTitle;
      procedure FixScroll(const ARange, APage, APos: integer);
      procedure GetSBVisibility(out HsbVisible: boolean); virtual;
      procedure GetSBRanges(const HsbVisible: boolean;
                    out HsbRange, HsbPage, HsbPos: Integer); virtual;
      procedure Paint; override;
      function  ScrollBarAutomatic(Which: TScrollStyle): boolean; virtual;
      function  ScrollBarIsVisible(Which:Integer): Boolean;
      procedure ScrollBarPosition(Which, Value: integer);
      procedure ScrollBarRange(Which:Integer; aRange,aPage,aPos: Integer);
      procedure ScrollBarShow(Which: Integer; aValue: boolean);
      procedure UpdateHorzScrollBar(const aVisible: boolean; const aRange,aPage,aPos: Integer); virtual;
      procedure WMHScroll(var message : TLMHScroll); message LM_HSCROLL;
      procedure WMLButtonDown(var message: TLMLButtonDown); message LM_LBUTTONDOWN;
    public
      constructor Create(AOwner: TvpGantt);  reintroduce;
      destructor Destroy; override;
      procedure InvalidateTitle;
  end;

  { TvpGanttCalendar }

  TvpGanttCalendar = class(TCustomControl)
    private
      FvpGantt: TvpGantt;
      //vars
      FCalendarWidth: integer;
      FCalendarHeight: integer;
      //scrollbars
      FVSbVisible: boolean;
      FHSbVisible: boolean;
      //private methods
      procedure UpdateSBVisibility;
      procedure UpdateSizes;
      //messages
      procedure WMSize(var Message: TLMSize);
        message LM_SIZE;
    protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure GetSBVisibility(out HsbVisible,VsbVisible:boolean);virtual;
      procedure Paint; override;
      procedure ScrollBarRange(Which:Integer; aRange,aPage,aPos: Integer);
      procedure ScrollBarPosition(Which, Value: integer);
      function  ScrollBarIsVisible(Which:Integer): Boolean;
      procedure ScrollBarPage(Which: Integer; aPage: Integer);
      procedure ScrollBarShow(Which: Integer; aValue: boolean);
      function  ScrollBarAutomatic(Which: TScrollStyle): boolean; virtual;
      procedure UpdateHorzScrollBar(const aVisible: boolean; const aRange,aPage,aPos: Integer); virtual;
      procedure UpdateVertScrollbar(const aVisible: boolean; const aRange,aPage,aPos: Integer); virtual;
      procedure VisualChange; virtual;
      procedure WMHScroll(var message : TLMHScroll); message LM_HSCROLL;
      procedure WMVScroll(var message : TLMVScroll); message LM_VSCROLL;
    public
      constructor Create(AOwner: TvpGantt);  reintroduce;
      destructor Destroy; override;
  end;

  {TvpGantt}
  TvpGantt = class(TCustomControl)
    private
      FvpGanttOptions: TvpGanttOptions;
      FvpGanttTasks: TvpGanttTasks;
      FvpGanttCalendar: TvpGanttCalendar;
      FSplitter: TSplitter;
      FScrollBars: TScrollStyle;

      FIntervals: TList;

      FUpdateCount: integer;

      FRowHeight: integer;
      FMajorScaleHeight: integer;
      FMinorScaleHeight: integer;
      FBorderWidth: integer;
      FBorderColor: TColor;
      FTaskColor: TColor;
      FCalendarColor: TColor;
      FFocusColor: TColor;
      FTitleFont: TFont;
      FTitleFontIsDefault: boolean;
      FTitleStyle: TTitleStyle;
      FFocusRow: integer;
      //methods
      function GetIntervalCount: Integer;
      function GetInterval(AnIndex: Integer): TvpInterval;
      function GetBorderWidth: integer;
      procedure OnTitleFontChanged(Sender: TObject);
      function OptionsIsStored: Boolean;
      procedure SetFocusColor(AValue: TColor);
      procedure SetMajorScaleHeight(AValue: integer);
      procedure SetMinorScaleHeight(AValue: integer);
      procedure SetOptions(AValue: TvpGanttOptions);
      procedure SetRowHeight(AValue: integer);
      procedure SetBorderWidth(AValue: integer);
      procedure SetBorderColor(AValue: TColor);
      procedure SetTaskColor(AValue: TColor);
      procedure SetCalendarColor(AValue: TColor);
      procedure SetScrollBars(const AValue: TScrollStyle);
      procedure SetTitleFont(const AValue: TFont);
      procedure SetTitleStyle(const AValue: TTitleStyle);
      procedure VisualChange;
      procedure WMSetFocus(var message: TLMSetFocus); message LM_SETFOCUS;
      procedure WMKillFocus(var message: TLMKillFocus); message LM_KILLFOCUS;
    protected
      function GetFocusRow: integer;
      function GetRowPosY(const YPos: integer): integer;
      function GetTitleHeight: integer;
      procedure FontChanged(Sender: TObject); override;
      procedure Paint; override;
      procedure SetFocusRow(AValue: integer);
      procedure SelectNextRow(const ADelta: integer);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property Interval[Index: Integer]: TvpInterval read GetInterval;
      property IntervalCount: Integer read GetIntervalCount;
      procedure AddInterval(AnInterval: TvpInterval);
      procedure InsertInterval(AnIndex: Integer; AnInterval: TvpInterval);
      procedure DeleteInterval(AnIndex: Integer);
      procedure RemoveInterval(AnInterval: TvpInterval);
      procedure UpdateInterval;

      procedure BeginUpdate;
      procedure EndUpdate(aRefresh: boolean = true);

      procedure First;
      procedure Last;
      procedure MoveTo(ARow: integer);
      property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;
    published
      property RowHeight: integer read FRowHeight write SetRowHeight default 19;
      property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
      property BorderColor: TColor read FBorderColor write SetBorderColor default clActiveBorder;
      property TaskColor:TColor read FTaskColor write SetTaskColor default clBtnFace;
      property CalendarColor:TColor read FCalendarColor write SetCalendarColor default clWindow;
      property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssAutoBoth;
      property MajorScaleHeight: integer read FMajorScaleHeight write SetMajorScaleHeight default 19;
      property MinorScaleHeight: integer read FMinorScaleHeight write SetMinorScaleHeight default 19;
      property TitleFont: TFont read FTitleFont write SetTitleFont;
      property TitleStyle: TTitleStyle read FTitleStyle write SetTitleStyle;
      property Options: TvpGanttOptions read FvpGanttOptions write SetOptions stored OptionsIsStored default DefaultGanttOptions;
  end;

  procedure DrawRubberRect(Canvas: TCanvas; aRect: TRect; Color: TColor);
  function  GetWorkingCanvas(const Canvas: TCanvas): TCanvas;
  procedure FreeWorkingCanvas(canvas: TCanvas);

resourcestring
  RS_TITLE_TASKS = 'Задача';

implementation

uses Unit1;

{$ifdef DbgScroll}
function SbToStr(Which: Integer): string;
begin
  case Which of
    SB_VERT: result := 'vert';
    SB_HORZ: result := 'horz';
    SB_BOTH: result := 'both';
    else
      result := '????';
  end;
end;

procedure DrawRubberRect(Canvas: TCanvas; aRect: TRect; Color: TColor);
  procedure DrawVertLine(X1,Y1,Y2: integer);
  begin
    if Y2<Y1 then
      while Y2<Y1 do begin
        Canvas.Pixels[X1, Y1] := Color;
        dec(Y1, constRubberSpace);
      end
    else
      while Y1<Y2 do begin
        Canvas.Pixels[X1, Y1] := Color;
        inc(Y1, constRubberSpace);
      end;
  end;
  procedure DrawHorzLine(X1,Y1,X2: integer);
  begin
    if X2<X1 then
      while X2<X1 do begin
        Canvas.Pixels[X1, Y1] := Color;
        dec(X1, constRubberSpace);
      end
    else
      while X1<X2 do begin
        Canvas.Pixels[X1, Y1] := Color;
        inc(X1, constRubberSpace);
      end;
  end;
begin
  with aRect do begin
    DrawHorzLine(Left, Top, Right-1);
    DrawVertLine(Right-1, Top, Bottom-1);
    DrawHorzLine(Right-1, Bottom-1, Left);
    DrawVertLine(Left, Bottom-1, Top);
  end;
end;

function GetWorkingCanvas(const Canvas: TCanvas): TCanvas;
var
  DC: HDC;
begin

  if (Canvas=nil) or (not Canvas.HandleAllocated) then begin
    DC := GetDC(0);
    Result := TCanvas.Create;
    Result.Handle := DC;
  end else
    Result := Canvas;

end;

procedure FreeWorkingCanvas(canvas: TCanvas);
begin
  ReleaseDC(0, Canvas.Handle);
  Canvas.Free;
end;


{$endif}



{ TvpInterval }

constructor TvpInterval.Create(AvpGantt: TvpGantt);
begin
  FvpGantt := AvpGantt;
  //initialize
end;

destructor TvpInterval.Destroy;
begin
  inherited Destroy;
end;

{ TvpGanttCalendar }

procedure TvpGanttCalendar.ScrollBarShow(Which: Integer; aValue: boolean);
begin
  if HandleAllocated then begin
    ShowScrollBar(Handle,Which,aValue);
    if Which in [SB_BOTH, SB_VERT] then FVSbVisible := AValue else
    if Which in [SB_BOTH, SB_HORZ] then FHSbVisible := AValue;
  end;
end;

function TvpGanttCalendar.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  result:=false;
  if (Which=ssVertical)or(Which=ssHorizontal) then begin
    if Which=ssVertical then Which:=ssAutoVertical
    else Which:=ssAutoHorizontal;
    Result:= FvpGantt.ScrollBars in [Which, ssAutoBoth];
  end;
end;

procedure TvpGanttCalendar.UpdateSBVisibility;
var
  HSbVisible, VSbVisible: boolean;
begin
  GetSBVisibility(HSbVisible, VSbVisible);
  ScrollBarShow(SB_VERT, VSbVisible);
  ScrollBarShow(SB_HORZ, HSbVisible);
end;

procedure TvpGanttCalendar.UpdateSizes;
begin
  //
end;

procedure TvpGanttCalendar.VisualChange;
begin
  {$ifdef DbgVisualChange}
  DebugLn('TCustomGrid.VisualChange INIT ',DbgSName(Self));
  {$endif}

  UpdateSizes;

  Invalidate;
  {$ifdef DbgVisualChange}
  DebugLn('TCustomGrid.VisualChange END ',DbgSName(Self));
  {$endif}
end;

procedure TvpGanttCalendar.WMHScroll(var message: TLMHScroll);
begin

end;

procedure TvpGanttCalendar.WMVScroll(var message: TLMVScroll);
begin

end;

procedure TvpGanttCalendar.WMSize(var Message: TLMSize);
begin
  inherited;
end;

procedure TvpGanttCalendar.ScrollBarRange(Which: Integer; aRange, aPage,
  aPos: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then begin
    {$Ifdef DbgScroll}
    DebugLn('ScrollbarRange: Which=%s Range=%d Page=%d Pos=%d',
      [SbToStr(Which),aRange,aPage,aPos]);
    {$endif}
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_DISABLENOSCROLL;
    //if not (gfPainting in FGridFlags) then
    //  ScrollInfo.fMask := ScrollInfo.fMask or SIF_POS;
    {$ifdef Unix}
    ScrollInfo.fMask := ScrollInfo.fMask or SIF_UPDATEPOLICY;
    if goThumbTracking in Options then
      ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS
    else
      ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;
    {$endif}
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := aRange;
    ScrollInfo.nPos := aPos;
    if APage<0 then
      APage := 0;
    ScrollInfo.nPage := APage;
    if (Which=SB_HORZ) and UseRightToLeftAlignment then begin
      ScrollInfo.nPos := ScrollInfo.nMax-ScrollInfo.nPage-ScrollInfo.nPos;
      {$Ifdef DbgScroll}
      DebugLn('ScrollbarRange: RTL nPos=%d',[ScrollInfo.nPos]);
      {$endif}
    end;
    SetScrollInfo(Handle, Which, ScrollInfo, True);
  end;
end;

procedure TvpGanttCalendar.ScrollBarPosition(Which, Value: integer);
var
  ScrollInfo: TScrollInfo;
  Vis: Boolean;
begin
  if HandleAllocated then begin
    {$Ifdef DbgScroll}
    DebugLn('ScrollbarPosition: Which=',SbToStr(Which), ' Value= ',IntToStr(Value));
    {$endif}
    if Which = SB_VERT then Vis := FVSbVisible else
    if Which = SB_HORZ then Vis := FHSbVisible
    else vis := false;
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    if (Which=SB_HORZ) and Vis and UseRightToLeftAlignment then begin
      ScrollInfo.fMask := SIF_PAGE or SIF_RANGE;
      GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
      Value := (ScrollInfo.nMax-ScrollInfo.nPage)-Value;
      {$Ifdef DbgScroll}
      DebugLn('ScrollbarPosition: Which=',SbToStr(Which), ' RTL Value= ',IntToStr(Value));
      {$endif}
    end;
    ScrollInfo.fMask := SIF_POS;
    ScrollInfo.nPos:= Value;
    SetScrollInfo(Handle, Which, ScrollInfo, Vis);
  end;
end;

function TvpGanttCalendar.ScrollBarIsVisible(Which: Integer): Boolean;
begin
  Result:=false;
  if HandleAllocated then begin
    {$IFNDEF MSWINDOWS}
    Result:= getScrollbarVisible(handle, Which);
    {$ELSE}
    // Is up to the widgetset to implement GetScrollbarvisible
    // FVSbVisible, FHSbVisible are supposed to be update (if used ScrolLBarShow)
    // how can we know if GetScrollbarVisible is indeed implemented?....
    if Which = SB_VERT then result := FVSbVisible else
    if Which = SB_HORZ then result := FHsbVisible else
    if Which = SB_BOTH then result := FHsbVisible and FVsbVisible;
    {$ENDIF}
  end;
end;

procedure TvpGanttCalendar.ScrollBarPage(Which: Integer; aPage: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then begin
    {$Ifdef DbgScroll}
    DebugLn('ScrollbarPage: Which=',SbToStr(Which), ' Avalue=',dbgs(aPage));
    {$endif}
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_PAGE;
    ScrollInfo.nPage:= aPage;
    SetScrollInfo(Handle, Which, ScrollInfo, True);
  end;
end;

procedure TvpGanttCalendar.UpdateHorzScrollBar(const aVisible: boolean;
  const aRange, aPage, aPos: Integer);
begin
  {$ifdef DbgScroll}
  DebugLn('TCustomGrid.UpdateHorzScrollbar: Vis=%s Range=%d Page=%d aPos=%d',
    [dbgs(aVisible),aRange, aPage, aPos]);
  {$endif}
  ScrollBarShow(SB_HORZ, aVisible);
  if aVisible then
    ScrollBarRange(SB_HORZ, aRange, aPage, aPos);
end;

procedure TvpGanttCalendar.UpdateVertScrollbar(const aVisible: boolean;
  const aRange, aPage, aPos: Integer);
begin
  {$ifdef DbgScroll}
  DebugLn('TCustomGrid.UpdateVertScrollbar: Vis=%s Range=%d Page=%d aPos=%d',
    [dbgs(aVisible),aRange, aPage, aPos]);
  {$endif}
  ScrollBarShow(SB_VERT, aVisible);
  if aVisible then
    ScrollbarRange(SB_VERT, aRange, aPage, aPos );
end;

procedure TvpGanttCalendar.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    WindowClass.Style := WindowClass.Style and DWORD(not ClassStylesOff);
    Style := Style or WS_VSCROLL or WS_HSCROLL or WS_CLIPCHILDREN;
  end;
end;

procedure TvpGanttCalendar.GetSBVisibility(out HsbVisible, VsbVisible: boolean);
var
  autoVert,autoHorz: boolean;
  ClientW,ClientH: Integer;
  BarW,BarH: Integer;
begin
  AutoVert := ScrollBarAutomatic(ssVertical);
  AutoHorz := ScrollBarAutomatic(ssHorizontal);

  //// get client bounds free of bars
  //ClientW  := ClientWidth;
  //ClientH  := ClientHeight;
  //BarW := GetSystemMetrics(SM_CXVSCROLL) +
  //        GetSystemMetrics(SM_SWSCROLLBARSPACING);
  //if ScrollBarIsVisible(SB_VERT) then
  //  ClientW := ClientW + BarW;
  //BarH := GetSystemMetrics(SM_CYHSCROLL) +
  //        GetSystemMetrics(SM_SWSCROLLBARSPACING);
  //if ScrollBarIsVisible(SB_HORZ) then
  //  ClientH := ClientH + BarH;
  //
  //// first find out if scrollbars need to be visible by
  //// comparing against client bounds free of bars
  //HsbVisible := (FScrollBars in [ssHorizontal, ssBoth]) or
  //              (AutoHorz and (FGCache.GridWidth>ClientW));
  //
  //VsbVisible := (FScrollBars in [ssVertical, ssBoth]) or
  //              (AutoVert and (FGCache.GridHeight>ClientH));
  //
  //// then for automatic scrollbars check if grid bounds are
  //// in some part of area occupied by scrollbars
  //if not HsbVisible and AutoHorz and VsbVisible then
  //  HsbVisible := FGCache.GridWidth  > (ClientW-BarW);
  //
  //if not VsbVisible and AutoVert and HsbVisible then
  //  VsbVisible := FGCache.GridHeight > (ClientH-BarH);
  //
  //if AutoHorz then
  //  HsbVisible := HsbVisible and not AutoFillColumns;
  //
  //// update new cached client values according to visibility
  //// of scrollbars
  //if HsbVisible then
  //  FGCache.ClientHeight := ClientH - BarH;
  //if VsbVisible then
  //  FGCache.ClientWidth := ClientW - BarW;
  //
  //{$ifdef dbgscroll}
  //DebugLn('TCustomGrid.GetSBVisibility:');
  //DebugLn(['  Horz=',HsbVisible,' GW=',FGCache.GridWidth,
  //  ' CW=',ClientWidth,' CCW=',FGCache.ClientWidth,' BarW=',BarW]);
  //DebugLn(['  Vert=',VsbVisible,' GH=',FGCache.GridHeight,
  //  ' CH=',ClientHeight,' CCH=',FGCache.ClientHeight,' BarH=',BarH]);
  //{$endif}
end;

procedure TvpGanttCalendar.Paint;
var
  r: TRect;
begin
  r := ClientRect;
  Canvas.Brush.Color := clRed;
  Canvas.FillRect(r);
end;

constructor TvpGanttCalendar.Create(AOwner: TvpGantt);
begin
  inherited create(AOwner);
  FvpGantt := AOwner;
end;

destructor TvpGanttCalendar.Destroy;
begin
  inherited Destroy;
end;

{ TvpGanttTasks }

procedure TvpGanttTasks.UpdateSizes;
begin
  Form1.Debug('TvpGanttTasks.UpdateSizes');
  if HandleAllocated then
    begin
      CalcTasksHeight;
      CalcTasksWidth;
      CalcScrollbarsRange;
    end;
end;

procedure TvpGanttTasks.VisualChange;
begin
  Form1.Debug('TvpGanttTasks.VisualChange');
  UpdateSizes;
  Invalidate;
end;

function TvpGanttTasks.GetTitleRect: TRect;
begin
  Form1.Debug('TvpGanttTasks.GetTitleRect');
  CalcTitleHeight;
  result := Rect(0, 0, ClientWidth, FTitleHeight);
end;

procedure TvpGanttTasks.WMSize(var Message: TLMSize);
begin
  Form1.Debug('TvpGanttTasks.WMSize');
  if not Self.IsResizing then
    begin
      UpdateSizes;
      inherited;
    end;
end;

procedure TvpGanttTasks.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_HREDRAW;
begin
  Form1.Debug('TvpGanttTasks.CreateParams');
  inherited CreateParams(Params);
  with Params do begin
    WindowClass.Style := WindowClass.Style and DWORD(not ClassStylesOff);
    Style := Style or WS_HSCROLL or WS_CLIPCHILDREN;
  end;
end;

function TvpGanttTasks.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Form1.Debug('TvpGanttTasks.DoMouseWheel');
  Form1.ShowParam(IntToStr(WheelDelta));
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TvpGanttTasks.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Form1.Debug('TvpGanttTasks.DoMouseWheelDown');
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    begin
      FvpGantt.SelectNextRow(1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

function TvpGanttTasks.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Form1.Debug('TvpGanttTasks.DoMouseWheelUp');
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    begin
      FvpGantt.SelectNextRow(-1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

procedure TvpGanttTasks.CalcFocusRect(var aRect: TRect);
begin
  Form1.Debug('TvpGanttTasks.CalcFocusRect');
  aRect.Left := 0;
  aRect.Right := Min(aRect.Right, ClientWidth);
  InflateRect(aRect, -1, 0);
  inc(aRect.Top, 1);
end;

function TvpGanttTasks.CalcRowRect(aRow: integer): TRect;
var
  aStart, aRowBorder: integer;
begin
  Form1.Debug('TvpGanttTasks.CalcRowRect');
  aRowBorder := FvpGantt.GetBorderWidth;
  {DONE Рассчитать правую границу в соответствии с ClientWidth
        было FTasksWidth + aRowBorder - FXScrollPosition  стало ClientWidth}
  // -1 потому что рисуем обводку по контуру списка задач
  aStart := FTitleHeight +
            (FvpGantt.RowHeight + aRowBorder) * aRow - 1;
  Result := Rect(0 - FXScrollPosition, aStart,
                 Self.ClientWidth - 1, aStart + FvpGantt.RowHeight + aRowBorder);
end;

procedure TvpGanttTasks.CalcTasksHeight;
var
  i: integer;
begin
  Form1.Debug('TvpGanttTasks.CalculateTasksHeight');
  FTasksHeight := 0;
  for i:=0 to FvpGantt.IntervalCount-1 do
    FTasksHeight := FTasksHeight + FvpGantt.RowHeight;
end;

procedure TvpGanttTasks.CalcTasksWidth;
var
  i, aBorder: integer;
begin
  Form1.Debug('TvpGanttTasks.CalculateTasksWidth');
  FTasksWidth := 0;
  aBorder := FvpGantt.GetBorderWidth;
  for i:=0 to FvpGantt.IntervalCount - 1 do
    FTasksWidth := Max(FTasksWidth, Canvas.TextWidth(FvpGantt.Interval[i].FName) );
  FTasksWidth := Max(FTasksWidth + 2*constCellPadding + aBorder, ClientWidth);
end;

procedure TvpGanttTasks.CalcTitleHeight;
begin
  Form1.Debug('TvpGanttTasks.CalcTitleHeight');
  if not Assigned(FvpGantt) then
    Exit;
  FTitleHeight := FvpGantt.GetTitleHeight;
end;

procedure TvpGanttTasks.CalcScrollbarsRange;
var
  HsbVisible: boolean;
  HsbRange, HsbPage, HsbPos: Integer;
begin
  Form1.Debug('TvpGanttTasks.CalcScrollbarsRange');
  GetSBVisibility(HsbVisible);
  GetSBRanges(HsbVisible, HsbRange, HsbPage, HsbPos);
  UpdateHorzScrollBar(HsbVisible, HsbRange, HsbPage, HsbPos);
end;

procedure TvpGanttTasks.DrawAllRows;
var
  i: integer;
begin
  Form1.Debug('TvpGanttTasks.DrawAllRows');
  for i:=0 to FvpGantt.IntervalCount-1 do
    DrawRow(i);
end;

procedure TvpGanttTasks.DrawEdges;
begin
  Form1.Debug('TvpGanttTasks.DrawEdges');
  //clear canvas
  Canvas.Brush.Color := FvpGantt.FTaskColor;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clActiveBorder;
  Canvas.Rectangle(ClientRect);
end;

procedure TvpGanttTasks.DrawFocusRect(aRow: integer; ARect: TRect);
begin
  Form1.Debug('TvpGanttTasks.DrawFocusRect');
  // Draw focused cell if we have the focus
  Form1.EL.Debug('Focused %d', [Integer(Focused)]);
  if FvpGantt.Focused then
    begin
      CalcFocusRect(aRect);
      if (vpgFocusHighlight in FvpGantt.Options) then
        begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
          Canvas.FillRect(aRect);
        end;
      //if FUseXORFeatures then begin
      //  Canvas.SaveHandleState;
      //  OldFocusColor := FFocusColor;
      //  FFocusColor:= clBlack;//White not visible on White background
      //  OldPenMode:=Canvas.Pen.Mode;
      //  Canvas.Pen.Mode := pmXOR;
      //end;
      DrawRubberRect(Canvas, aRect, FvpGantt.FocusColor);
      //if FUseXORFeatures then begin
      //  Canvas.Pen.Mode := OldPenMode;
      //  Canvas.RestoreHandleState;
      //  FFocusColor := OldFocusColor;
      //end;
    end;
end;

procedure TvpGanttTasks.DrawRow(aRow: integer);
var
  rRect: TRect;
begin
  Form1.Debug('TvpGanttTasks.DrawRow');
  if not Assigned(FvpGantt) then Exit;
  rRect := CalcRowRect(aRow);
  if rRect.Top<ClientHeight then
    begin
      Form1.EL.Debug('DrawRow %d', [aRow]);
      Canvas.Font := Font;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := FvpGantt.BorderColor;
      Canvas.MoveTo(rRect.Left, rRect.Bottom);
      Canvas.LineTo(rRect.Right, rRect.Bottom);
      Canvas.LineTo(rRect.Right, rRect.Top);
      //focus
      Form1.EL.Debug('FvpGantt.GetFocusRow');
      if aRow=FvpGantt.GetFocusRow then
        DrawFocusRect(aRow, rRect);
      //выводим текст
      Canvas.TextRect(rRect, rRect.Left + constCellPadding, rRect.Top + constCellPadding, FvpGantt.Interval[aRow].Name);
    end;
end;

procedure TvpGanttTasks.DrawTitle;
var
  titRect: TRect;
begin
  Form1.Debug('TvpGanttTasks.DrawTitle');
  if not Assigned(FvpGantt) then Exit;
  titRect := GetTitleRect;
  //заливаем
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(titRect);
  //рисуем границу
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clActiveBorder;
  Canvas.Rectangle(titRect);
  //пишем текст
  if Assigned(FvpGantt) then
    Canvas.Font.Assign(FvpGantt.TitleFont)
  else
    Canvas.Font := Font;
  InflateRect(titRect, -1, -1);
  DrawText(Canvas.Handle, PChar(RS_TITLE_TASKS), Length(RS_TITLE_TASKS), titRect, DT_CENTER OR DT_SINGLELINE OR DT_VCENTER);
end;

procedure TvpGanttTasks.FixScroll(const ARange, APage, APos: integer);
var
  maxPos: integer;
begin
   Form1.Debug('TvpGanttTasks.FixScroll');
  //при скролинге позиция считается как Max-Page, поэтому нажо найти сначала
  //коэффициент отношения размера скрытой области к значениям положения ползунка
  //ну, а в конце поправить неточность округления
  maxPos := aRange - MAX(APage - 1, 0);
  FXScrollPosition := Ceil(aRange/maxPos*aPos);
  //if APos = maxPos then
  //  inc(FXScrollPosition, 2*constCellPadding + 2*FvpGantt.GetBorderWidth);
end;

procedure TvpGanttTasks.GetSBVisibility(out HsbVisible: boolean);
var
  autoHorz: boolean;
  ClientW, ClientH: Integer;
  BarH: Integer;
begin
  Form1.Debug('TvpGanttTasks.GetSBVisibility');
  AutoHorz := ScrollBarAutomatic(ssHorizontal);

  // get client bounds free of bars
  ClientW  := ClientWidth;
  ClientH  := ClientHeight;
  BarH := GetSystemMetrics(SM_CYHSCROLL) +
          GetSystemMetrics(SM_SWSCROLLBARSPACING);
  if ScrollBarIsVisible(SB_HORZ) then
    ClientH := ClientH + BarH;

  // first find out if scrollbars need to be visible by
  // comparing against client bounds free of bars
  HsbVisible := (FvpGantt.ScrollBars in [ssBoth, ssHorizontal]) or
                (AutoHorz and (FTasksWidth>ClientW));

  // then for automatic scrollbars check if grid bounds are
  // in some part of area occupied by scrollbars
  if not HsbVisible and AutoHorz then
    HsbVisible := FTasksWidth  > ClientW;

  if AutoHorz then
    HsbVisible := HsbVisible; // and not AutoFillColumns;

  // update new cached client values according to visibility
  // of scrollbars
  if HsbVisible then
    FTasksHeight := ClientH - BarH;
end;

procedure TvpGanttTasks.GetSBRanges(const HsbVisible: boolean; out
  HsbRange, HsbPage, HsbPos: Integer);
begin
  Form1.Debug('TvpGanttTasks.GetSBRanges');
  //calculate only horizontal
  HsbRange := 0;
  HsbPos := 0;
  if HsbVisible then
    begin
      HsbRange := Max(0, FTasksWidth - ClientWidth);
      HsbPage := Min(SCROLL_PAGE_DEFAULT, FTasksWidth - ClientWidth - 1);
    end;
  Form1.Debug(Format('HsbRange %d', [HsbRange]));
  Form1.Debug(Format('FTasksWidth %d', [FTasksWidth]));
end;

procedure TvpGanttTasks.Paint;
begin
  Form1.Debug('TvpGanttTasks.Paint');
  DrawEdges;
  DrawTitle;
  DrawAllRows;
end;

function TvpGanttTasks.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  Form1.Debug('TvpGanttTasks.ScrollBarAutomatic');
  result := false;
  if (Which=ssVertical) OR (Which=ssHorizontal) then
    begin
      if Which=ssVertical then
        Which := ssAutoVertical
      else
        Which := ssAutoHorizontal;
      Result := FvpGantt.ScrollBars in [Which, ssAutoBoth];
    end;
end;

function TvpGanttTasks.ScrollBarIsVisible(Which: Integer): Boolean;
begin
  Form1.Debug('TvpGanttTasks.ScrollBarIsVisible');
  Result:=false;
  if HandleAllocated then begin
    {$IFNDEF MSWINDOWS}
    Result:= getScrollbarVisible(handle, Which);
    {$ELSE}
    // Is up to the widgetset to implement GetScrollbarvisible
    // FVSbVisible, FHSbVisible are supposed to be update (if used ScrolLBarShow)
    // how can we know if GetScrollbarVisible is indeed implemented?....
    if Which = SB_HORZ then
      result := FHsbVisible
    else if Which = SB_BOTH then result := FHsbVisible;
    {$ENDIF}
  end;
end;

procedure TvpGanttTasks.ScrollBarPosition(Which, Value: integer);
var
  ScrollInfo: TScrollInfo;
  aVisible: Boolean;
begin
  Form1.Debug('TvpGanttTasks.ScrollBarPosition');
  if HandleAllocated then begin
    if Which = SB_HORZ then aVisible := FHSbVisible
    else aVisible := false;
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_POS;
    ScrollInfo.nPos:= Value;
    SetScrollInfo(Handle, Which, ScrollInfo, aVisible);
  end;
end;

procedure TvpGanttTasks.ScrollBarRange(Which: Integer; aRange, aPage,
  aPos: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  Form1.Debug('TvpGanttTasks.ScrollBarRange');
  if HandleAllocated then begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := aRange;
    ScrollInfo.nPos := aPos;
    if APage<0 then
      APage := 0;
    ScrollInfo.nPage := APage;
    SetScrollInfo(Handle, Which, ScrollInfo, True);
  end;
end;

procedure TvpGanttTasks.ScrollBarShow(Which: Integer; aValue: boolean);
begin
  Form1.Debug('TvpGanttTasks.ScrollBarShow');
  if HandleAllocated then begin
    ShowScrollBar(Handle,Which,aValue);
    if Which in [SB_BOTH, SB_HORZ] then FHSbVisible := AValue;
  end;
end;

procedure TvpGanttTasks.UpdateHorzScrollBar(const aVisible: boolean;
  const aRange, aPage, aPos: Integer);
begin
  Form1.Debug('TvpGanttTasks.UpdateHorzScrollBar');
  ScrollBarShow(SB_HORZ, aVisible);
  if aVisible then
    ScrollBarRange(SB_HORZ, aRange, aPage, aPos);
end;

procedure TvpGanttTasks.WMHScroll(var message: TLMHScroll);
var
  aPos, maxPos: Integer;
  ScrollInfo: TScrollInfo;
  aCode: Smallint;
begin
  Form1.Debug('TvpGanttTasks.WMHScroll');
  if not HandleAllocated then
    exit;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_POS;
  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  maxPos := ScrollInfo.nMax - Max(ScrollInfo.nPage-1, 0);

  aCode := message.ScrollCode;
  aPos := ScrollInfo.nPos;

  Form1.Debug(Format('maxPos %d', [maxPos]));

  Form1.Debug(Format('aPos %d', [aPos]));
  case aCode of
    SB_LEFT:
      aPos := ScrollInfo.nMin;
    SB_RIGHT:
      aPos := maxPos;
    // Scrolls one line left / right
    SB_LINERIGHT:
      if aPos < maxPos then
        inc(aPos);
    SB_LINELEFT:
      if aPos > ScrollInfo.nMin then
        dec(aPos);
    // Scrolls one page of lines up / down
    SB_PAGERIGHT:
      begin
        if (aPos + ScrollInfo.nPage)>maxPos then
          aPos := maxPos
        else
          aPos := aPos + (ScrollInfo.nPage - 1);
      end;
    SB_PAGELEFT:
      begin
        if (aPos - ScrollInfo.nPage) < ScrollInfo.nMin then
          aPos := ScrollInfo.nMin
        else
          aPos := aPos - (ScrollInfo.nPage + 1);
      end;
    // Scrolls to the current scroll bar position
    SB_THUMBPOSITION:
      aPos := message.Pos;
    SB_THUMBTRACK:
      aPos := message.Pos;
    // Ends scrolling
    SB_ENDSCROLL:
      Exit;
  end;

  Form1.Debug(Format('aPos %d', [aPos]));
  ScrollBarPosition(SB_HORZ, aPos);
  FixScroll(ScrollInfo.nMax, ScrollInfo.nPage, aPos);
  Form1.Debug(Format('FScrollPosition %d', [FXScrollPosition]));
  //  if EditorMode then
  //    EditorPos;

  Invalidate;
end;

procedure TvpGanttTasks.WMLButtonDown(var message: TLMLButtonDown);
var
  aRow: integer;
begin
  Form1.Debug('TvpGanttTasks.WMLButtonDown');
  Form1.EL.Debug('x %d y %d',[message.XPos, message.YPos]);
  inherited;
  //TODO Вычесть значение прокрутки по Y
  aRow := FvpGantt.GetRowPosY(message.YPos);
  if aRow>-1 then
    FvpGantt.SetFocusRow(aRow);
  FvpGantt.SetFocus;
end;

constructor TvpGanttTasks.Create(AOwner: TvpGantt);
begin
  Form1.Debug('TvpGanttTasks.Create');
  inherited create(AOwner);
  FvpGantt := AOwner;
  //default value
  FXScrollPosition := 0;
end;

destructor TvpGanttTasks.Destroy;
begin
  Form1.Debug('TvpGanttTasks.Destroy');
  inherited Destroy;
end;

procedure TvpGanttTasks.InvalidateTitle;
var
  R: TRect;
begin
  Form1.Debug('TvpGanttTasks.InvalidateTitle');
  if not HandleAllocated then
    exit;
  R := GetTitleRect;
  InvalidateRect(Handle, @R, False);
end;

{ TvpGantt }

function TvpGantt.GetIntervalCount: Integer;
begin
  Result := 0;
  if Assigned(FIntervals) then
    Result := FIntervals.Count;
end;

function TvpGantt.GetInterval(AnIndex: Integer): TvpInterval;
begin
  Result := TvpInterval(FIntervals[AnIndex]);
end;

procedure TvpGantt.OnTitleFontChanged(Sender: TObject);
begin
  Form1.Debug('TvpGantt.OnTitleFontChanged');
  FTitleFontIsDefault := False;
  VisualChange;
end;

function TvpGantt.OptionsIsStored: Boolean;
begin
  Result := FvpGanttOptions <> DefaultGanttOptions;
end;

procedure TvpGantt.SetFocusColor(AValue: TColor);
begin
  Form1.Debug('TvpGantt.SetFocusColor');
  if FFocusColor = AValue then Exit;
  FFocusColor := AValue;
  //TODO Сделать обновление области с фокусом
end;

procedure TvpGantt.SetMajorScaleHeight(AValue: integer);
begin
  Form1.Debug('TvpGantt.SetMajorScaleHeight');
  if FMajorScaleHeight = AValue then Exit;
  FMajorScaleHeight := AValue;
  VisualChange;
end;

procedure TvpGantt.SetMinorScaleHeight(AValue: integer);
begin
  Form1.Debug('TvpGantt.SetMinorScaleHeight');
  if FMinorScaleHeight = AValue then Exit;
  FMinorScaleHeight := AValue;
  VisualChange;
end;

procedure TvpGantt.SetOptions(AValue: TvpGanttOptions);
begin
  Form1.Debug('TvpGantt.SetOptions');
  if FvpGanttOptions = AValue then Exit;
  FvpGanttOptions := AValue;
  VisualChange;
end;

procedure TvpGantt.SetRowHeight(AValue: integer);
begin
  Form1.Debug('TvpGantt.SetRowHeight');
  if FRowHeight = AValue then Exit;
  FRowHeight := AValue;
  VisualChange;
end;

procedure TvpGantt.SetBorderWidth(AValue: integer);
begin
  Form1.Debug('TvpGantt.SetBorderWidth');
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;
  VisualChange;
end;

procedure TvpGantt.SetBorderColor(AValue: TColor);
begin
  Form1.Debug('TvpGantt.SetBorderColor');
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;
  VisualChange;
end;

procedure TvpGantt.SetTaskColor(AValue: TColor);
begin
  Form1.Debug('TvpGantt.SetTaskColor');
  if FTaskColor=AValue then
    Exit;
  FTaskColor := AValue;
  if FUpdateCount>0 then
    FvpGanttTasks.Repaint;
end;

procedure TvpGantt.SetCalendarColor(AValue: TColor);
begin
  Form1.Debug('TvpGantt.SetCalendarColor');
  if FCalendarColor=AValue then
    Exit;
  FCalendarColor := AValue;
  if (FUpdateCount>0) AND FvpGanttCalendar.HandleAllocated then
    FvpGanttCalendar.Repaint;
end;

procedure TvpGantt.SetScrollBars(const AValue: TScrollStyle);
begin
  Form1.Debug('TvpGantt.SetScrollBars');
  if FScrollBars=AValue then
    Exit;
  FScrollBars := AValue;
  VisualChange;
end;

procedure TvpGantt.SetTitleFont(const AValue: TFont);
begin
  Form1.Debug('TvpGantt.SetTitleFont');
  FTitleFont.Assign(AValue);
  VisualChange;
end;

procedure TvpGantt.SetTitleStyle(const AValue: TTitleStyle);
begin
  Form1.Debug('TvpGantt.SetTitleStyle');
  if FTitleStyle=AValue then exit;
  FTitleStyle:=AValue;
  Invalidate;
end;

procedure TvpGantt.VisualChange;
begin
  Form1.Debug('TvpGantt.VisualChange');
  if HandleAllocated then
    begin
      if FvpGanttTasks.HandleAllocated then
        FvpGanttTasks.VisualChange;
      if FvpGanttCalendar.HandleAllocated then
        FvpGanttCalendar.VisualChange;
    end;
end;

procedure TvpGantt.WMSetFocus(var message: TLMSetFocus);
begin
  Form1.Debug('TvpGantt.WMSetFocus');
  VisualChange;
end;

procedure TvpGantt.WMKillFocus(var message: TLMKillFocus);
var
  R: TRect;
begin
  Form1.Debug('TvpGantt.WMKillFocus');
  if FvpGanttTasks.HandleAllocated then
    begin
      R := FvpGanttTasks.CalcRowRect(FFocusRow);
      InvalidateRect(FvpGanttTasks.Handle, @R, true);
    end;
end;

function TvpGantt.GetBorderWidth: integer;
begin
  if FBorderWidth > 0 then
    Result := FBorderWidth
  else
    Result := 0;
end;

procedure TvpGantt.Paint;
begin
  //отрисовка :)
end;

procedure TvpGantt.FontChanged(Sender: TObject);
begin
  if csCustomPaint in ControlState then
    Canvas.Font := Font
  else begin
    inherited FontChanged(Sender);
    if FTitleFontIsDefault then begin
      FTitleFont.Assign(Font);
      FTitleFontIsDefault := True;
    end;
  end;
end;

function TvpGantt.GetFocusRow: integer;
begin
  Form1.Debug('TvpGantt.GetFocusRow');
  Result := FFocusRow;
end;

procedure TvpGantt.SetFocusRow(AValue: integer);
begin
  Form1.Debug('TvpGantt.SetFocusRow');
  if (FFocusRow=AValue) OR (AValue>=FIntervals.Count) then
    Exit;
  FFocusRow := AValue;
  Invalidate;
end;

procedure TvpGantt.SelectNextRow(const ADelta: integer);
var
  aRow: integer;
begin
  Form1.Debug('TvpGantt.SelectNextRow');
  if FFocusRow>=IntervalCount-1 then
    Exit;
  aRow := FFocusRow + ADelta;

  if (FFocusRow>=0) AND (FFocusRow<IntervalCount-1) then
    begin
      aRow := FFocusRow + ADelta;
      SetFocusRow(aRow);
    end;
end;

function TvpGantt.GetRowPosY(const YPos: integer): integer;
var
  aTH: integer;
begin
  Form1.Debug('TvpGantt.GetRowPosXY');
  {DONE Отработать правильное нажатие на титле, иначе ниже середины получаем 0 строку и соответственно
        переход на первую строку}
  {TODO Сделать обработку вычисления положения курсора на заголовке}
  aTH := GetTitleHeight;
  if YPos > aTH then
    Result := Trunc((YPos - GetTitleHeight) / (RowHeight + GetBorderWidth))
  else
    Result := -1;
end;


constructor TvpGantt.Create(AOwner: TComponent);
begin
  Form1.Debug('TvpGantt.Create');
  inherited Create(AOwner);
  TabStop := true;
  Form1.EL.Debug('TabStop %d', [Integer(TabStop)]);

  FIntervals := TList.Create;

  //create vpGanttTasks
  FvpGanttTasks := TvpGanttTasks.Create(Self);
  InsertControl(FvpGanttTasks);
  FvpGanttTasks.Width := C_DEFAULT_TASKS_WIDTH;
  FvpGanttTasks.Align := alLeft;

  //create splitter
  FSplitter := TSplitter.Create(Self);
  InsertControl(FSplitter);
  FSplitter.Left := FvpGanttTasks.Left + FvpGanttTasks.Width + 1;
  FSplitter.Align := alLeft;
  FSplitter.Width := C_DEFAULT_SPLITTER_WIDTH;

  //create vpGanttTasks
  FvpGanttCalendar := TvpGanttCalendar.Create(Self);
  InsertControl(FvpGanttCalendar);
  FvpGanttCalendar.Align := alClient;

  BeginUpdate;

  Options := DefaultGanttOptions;

  Width := C_VPGANTT_WIDTH;
  Height := C_VPGANTT_HEIGHT;

  BorderWidth := C_DEFAULT_BORDER_WIDTH;
  TaskColor := clWindow;
  CalendarColor := clWindow;
  BorderColor := clActiveBorder;
  ScrollBars := ssAutoBoth;

  RowHeight := C_DEFAULT_ROW_HEIGHT;
  MajorScaleHeight := C_DEFAULT_ROW_HEIGHT;
  MinorScaleHeight := C_DEFAULT_ROW_HEIGHT;

  FTitleFont := TFont.Create;
  FTitleFont.OnChange := @OnTitleFontChanged;
  FTitleFont.Style := [fsBold];
  FTitleFontIsDefault := True;

  FFocusRow := -1;
  FFocusColor := clBlack;

  EndUpdate();
end;

destructor TvpGantt.Destroy;
var
  i: Integer;
begin
  try
    try
      for i := 0 to FIntervals.Count-1 do
        TvpInterval(FIntervals.Items[i]).Free;
    except
    end;
  finally
    FvpGanttCalendar.Free;
    FSplitter.Free;
    FvpGanttTasks.Free;
    FreeAndNil(FIntervals);
    FreeThenNil(FTitleFont);
  end;
  inherited Destroy;
end;


procedure TvpGantt.AddInterval(AnInterval: TvpInterval);
begin
  FIntervals.Add(AnInterval);
  SetFocusRow(FIntervals.Count-1);
  UpdateInterval;
end;

procedure TvpGantt.InsertInterval(AnIndex: Integer; AnInterval: TvpInterval);
begin
  FIntervals.Insert(AnIndex, AnInterval);
  SetFocusRow(AnIndex);
  UpdateInterval;
end;

procedure TvpGantt.DeleteInterval(AnIndex: Integer);
begin
  FIntervals.Delete(AnIndex);
  if FFocusRow>AnIndex then
    SetFocusRow(FFocusRow-1);
  UpdateInterval;
end;

procedure TvpGantt.RemoveInterval(AnInterval: TvpInterval);
var
  AnIndex: integer;
begin
  AnIndex := FIntervals.IndexOf(AnInterval);
  FIntervals.Remove(AnInterval);
  if FFocusRow>AnIndex then
    SetFocusRow(FFocusRow-1);
  UpdateInterval;
end;

procedure TvpGantt.UpdateInterval;
begin
  Form1.Debug('TvpGantt.UpdateInterval');
  VisualChange;
end;

procedure TvpGantt.BeginUpdate;
begin
  Form1.Debug('TvpGantt.BeginUpdate');
  Inc(FUpdateCount);
end;

procedure TvpGantt.EndUpdate(aRefresh: boolean);
begin
  Form1.Debug('TvpGantt.EndUpdate');
  Dec(FUpdateCount);
  if (FUpdateCount=0) and aRefresh then
    VisualChange;
end;

procedure TvpGantt.First;
begin
  Form1.Debug('TvpGantt.First');
  SetFocusRow(0);
end;

procedure TvpGantt.Last;
begin
  Form1.Debug('TvpGantt.Last');
  SetFocusRow(FIntervals.Count-1);
end;

procedure TvpGantt.MoveTo(ARow: integer);
begin
  Form1.Debug('TvpGantt.MoveTo');
  SetFocusRow(ARow);
end;

function TvpGantt.GetTitleHeight: integer;
begin
  Form1.Debug('TvpGantt.GetTitleHeight');
  if not Assigned(FvpGanttTasks) then
    Result := -1;
  Result := FMajorScaleHeight + FMinorScaleHeight;
end;


end.

