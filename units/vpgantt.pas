{
/******************************************************************************/
                            vpGantt.pas
                            -----------
                             ver. 0.3b

                     Gantt Diagramm component unit
                Drawing a gantt diagramm from an interval list.
--------------------------------------------------------------------------------

  @git: https://github.com/Phantovas/vpGantt

  @clone on https: https://github.com/Phantovas/vpGantt.git
  @clone on ssh: git@github.com:Phantovas/vpGantt.git

  @Author: Vasiliy Ponomarjov
  @Email: phantovas@gmail.com
  @created: 06-JAN-2019
  @modified: 06-FEB-2019
  @version: migrate to 0.4a

/******************************************************************************/

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

/******************************************************************************/
}


{ TODO -o Vas:
  1. Интервалы сделать как коллекцию. С возможностью задания для каждого интервала своих свойств
  2. Подумать о группах.
  3. ЕСли будут группы, то тогда можно UpdateDates делать по группам, а в группах обновлять максимальную и минимальные даты
     при добавлении интервалов.
  4. Сделать сортировку по датам
}

unit vpGantt;

{$mode ObjFPC}

//{$define DBGINTERVAL}
//{$define DBGGANTT}
//{$define DBGGANTTTASKS}
//{$define DBGGANTTCALENDAR}
//{$define DBGSCROLL}
//{$define DBGDRAW}
//{$define DBGFOCUS}

interface

uses
  LCLproc, LCLIntf, LCLType, LMessages, SysUtils, Classes, Controls,
  StdCtrls, Forms, ExtCtrls,
  //require
  Graphics, GraphUtil, Math, dateutils, Themes,
  //temp
  Dialogs;

const
  C_DEF_SPLITTER_WIDTH = 2;
  C_DEF_TASKS_WIDTH = 150;
  C_DEF_BORDER_WIDTH = 1;
  C_DEF_ROW_HEIGHT = 24;
  C_DEF_PIXEL_PER_MINOR = 30;
  C_VPGANTT_WIDTH = 300;
  C_VPGANTT_HEIGHT = 150;
  C_TITLE_TEXT_INDENT = 3;
  C_ROW_HIGHLIGHT_VALUE = 50;
  C_DEF_INTERVAL_PADDING = 3;
  C_DEF_INTERVAL_RADIUS = 3;
  C_FACT_SHADOW_VALUE = -25;
  C_CURTIMELINE_WIDTH = 2;
  C_CURTIMELINE_COLOR = clRed;

type
  TTitleStyle = (tsLazarus, tsStandard, tsNative);

  TvpgOption = (vpgDrawFocusSelected,         //рисовать всегда выделение
                vpgFocusHighlight,            //подсвечивать фокус, а не только рамку рисовать
                vpgMajorVertLine,             //рисовать вертикальные линии мажорной шкалы
                vpgMinorVertLine,             //рисовать вертикальные линии минорной шкалы
                vpgRowHighlight,              //рисовать подсветку всей строки
                vpgMoveFocusToNewInterval,    //перемещать фокус на добавленную строку
                vpgVertLine,                  //вертикальный бордюр в сетке
                vpgHorzLine,                  //горизонтальный бордюр в сетке
                vpgExtendVertLines,           //вертикальные разделители в высоту компонента
                vpgRowHint,                   //всплывающие подсказки для каждой строки
                vpgTitleTextNoScroll,         //всегда показывать надписи в заголовках
                vpgShowCurrentTime,           //показывать текущее время
                vpgScrollToCurrentTime        //автоматически прокручивать при обновлении на текущее время
                );

  TvpGanttOptions = set of TvpgOption;

  TvpTimeScale = (vptsMinute,                 //vptsMinute - минуты,
                  vptsDecMinute,              //vptsDecMinute - десятки минут (10..60),
                  vptsHour,                   //vptsHour - часы,
                  vptsDay,                    //vptsDay - дни,
                  vptsDayWeek,                //vptsDayWeek - дни недели,
                  vptsWeek,                   //vptsWeek - недели (дата.год),
                  vptsWeekNum,                //vptsWeekNum - номер недели,
                  vptsWeekNumPlain,           //vptsWeekNumPlain - название недели,
                  vptsMonth,                  //vptsMonth - месяцы,
                  vptsQuarter,                //vptsQuarter - кварталы,
                  vptsHalfYear,               //vptsHalfYear - полугодие,
                  vptsYear);                  //vptsYear - год

const
  constCellPadding: byte = 3;
  constRubberSpace: byte = 2;
  DefaultGanttOptions = [vpgDrawFocusSelected,
                         vpgFocusHighlight,
                         vpgMajorVertLine,
                         vpgRowHighlight,
                         vpgHorzLine,
                         vpgVertLine,
                         vpgRowHint,
                         vpgTitleTextNoScroll,
                         vpgShowCurrentTime
                        ];

resourcestring
  RS_TITLE_TASKS = 'Задача';
  //Scale
  RS_E_MAJORSCALE_LOW = 'MajorScale should by higher than MinorScale';
  RS_E_MAJORSCALE_EQUAL = 'MajorScale should by different from MinorScale';
  RS_E_MINORSCALE_HIGH = 'MinorScale should by lower than MajorScale';
  RS_E_MINORSCALE_EQUAL = 'MinorScale should by different from MajorScale';
  RS_E_LIST_INDEX_OUT_OF_BOUNDS = 'List index out of bounds';
  RS_E_FINISHDATE_LESS = 'Finish date should by higher than start date of the interval';
  RS_E_STARTDATE_HIGH = 'Start date shoul be lower than finish date of the interval';
  //date
  RS_HOUR = 'ч.';
  RS_WEEK = 'Нед. ';
  RS_KW = 'Кв. ';
  //хинты
  RS_HINT_STARTDATE = 'Начало: ';
  RS_HINT_DURATION = 'Продолжительность: ';
  RS_HINT_FINISHDATE = 'Окончание: ';
  RS_HINT_COMPLETE = 'Выполнено за: ';

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
    FDuration: Double;
    FTag: integer;
  public
    property Name: string read FName write FName;
    property Project: string read FProject write FProject;
    property Resource: string read FResource write FResource;
    property Tag: integer read FTag write FTag;
  end;

  { TvpInterval }

  TvpInterval = class(TvpBaseInterval)
    private
      FvpGantt: TvpGantt;
      FPlanRect: TRect;  //область планируемого времени
      FFactRect: TRect;  //область фактического времени
      function GetComplete: Double;
      function GetFinishDate: TDateTime;
      procedure SetDuration(AValue: Double);
      procedure SetFinishDate(AValue: TDateTime);
      procedure SetStartDate(AValue: TDateTime);
      procedure SetBoundHeight(const ATop, ABottom: integer);
    protected
      procedure CalcPlanBound;
      procedure CalcFactBound;
    public
      constructor Create(AvpGantt: TvpGantt); virtual;
      destructor Destroy; override;

      function IsExpired: boolean;
      procedure UpdateBounds;
    published
      property StartDate: TDateTime read FStartDate write SetStartDate;
      property FinishDate: TDateTime read GetFinishDate write SetFinishDate;
      property Duration: Double read FDuration write SetDuration;
      property Complete: Double read GetComplete;
  end;

  { TvpGanttTasks }

  TvpGanttTasks = class(TCustomControl)
    private
      //vars
      {TODO: -o Vas Сменить высоту и ширину на TRect}
      FTasksWidth: integer;
      FTasksHeight: integer; //нужно для хранения высоты всей области
      FGridVisibleRect: TRect; //область видимых строк
      FGridTextHeight: integer;
      FColor: TColor;
      //scrollbars
      FHScrollPosition: integer;
      FHSbVisible: boolean;
      //methods
      procedure CalcGridVisibleRect;
      function CalcRowRect(aRow: integer): TRect;
      procedure CalcTasksWidth;
      procedure CalcTasksHeight;
      procedure CalcScrollbarsRange;
      function GetTitleRect: TRect;
      procedure VisualChange; virtual;
      procedure UpdateSizes;
      //messages
      procedure WMSize(var Message: TLMSize); message LM_SIZE;
    protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure ClearCanvas;
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure DrawRows;
      procedure DrawEdges;
      procedure DrawRow(aRow: integer);
      procedure DrawTitle;
      procedure GetSBVisibility(out HsbVisible: boolean);
      procedure GetSBRanges(const HsbVisible: boolean;
                    out HsbRange, HsbPage, HsbPos: Integer);
      procedure InvalidateRow(aRow: integer);
      procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
      procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
      procedure Paint; override;
      function  ScrollBarIsVisible(Which:Integer): Boolean;
      procedure ScrollBarPosition(Which, Value: integer);
      procedure ScrollBarRange(Which:Integer; aRange,aPage,aPos: Integer);
      procedure ScrollBarShow(Which: Integer; aValue: boolean);
      procedure UpdateHorzScrollBar(const aVisible: boolean; const aRange,aPage,aPos: Integer);
      procedure WMHScroll(var message : TLMHScroll); message LM_HSCROLL;
    public
      constructor Create(AOwner: TvpGantt);  reintroduce;
      destructor Destroy; override;
      procedure InvalidateTitle;
  end;

  { TvpGanttCalendar }

  TvpGanttCalendar = class(TCustomControl)
    private
      //vars
      FCalendarWidth: integer;
      FCalendarHeight: integer;
      FGridVisibleRect: TRect; //область видимых строк
      FMajorScale: TvpTimeScale;
      FMinorScale: TvpTimeScale;
      FMajorScaleCount: integer;
      FMinorScaleCount: integer;
      FMinorCountInMajor: array of integer;
      FMajorScaleTitleRect: TRect;
      FMinorScaleTitleRect: TRect;
      FPixelePerMinorScale: integer;
      FGridTextHeight: integer;
      FColor: TColor;
      FFreeColor: TColor;
      FExpiredColor: TColor;
      //scrollbars
      FHScrollPosition: integer;
      FVSbVisible: boolean;
      FHSbVisible: boolean;
      //methods
      procedure CalcCalendarHeight;
      procedure CalcCalendarWidth;
      procedure CalcGridVisibleRect;
      function CalcFocusRect: TRect;
      function CalcRowRect(const aRow: integer): TRect;
      procedure CalcScaleCount;
      function GetMajorScaleHeight: integer;
      function GetMajorScaleWidth(const aCurItem: integer): integer;
      function GetMinorScaleHeight: integer;
      function GetMinorScaleWidth: integer;
      procedure UpdateSBVisibility; deprecated;
      procedure UpdateSizes;
      //messages
      procedure WMSize(var Message: TLMSize); message LM_SIZE;
    protected
      procedure CalcScrollbarsRange;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure ClearCanvas;
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure DrawCurrentTimeLine;
      procedure DrawEdges;
      procedure DrawInterval(const aRow: integer);
      procedure DrawMajorScale;
      procedure DrawMinorScale;
      procedure DrawRow(const aRow: integer);
      procedure DrawRows;
      procedure DrawScaleVertLines(const X: integer);
      procedure GetSBVisibility(out HsbVisible,VsbVisible:boolean);
      procedure GetSBRanges(const HsbVisible,VsbVisible: boolean;
                    out HsbRange,VsbRange,HsbPage,VsbPage,HsbPos,VsbPos:Integer);
      procedure InvalidateRow(aRow: integer);
      procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
      procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
      procedure Paint; override;
      procedure ScrollBarRange(Which:Integer; aRange,aPage,aPos: Integer);
      procedure ScrollBarPosition(Which, Value: integer);
      function  ScrollBarIsVisible(Which:Integer): Boolean;
      procedure ScrollBarPage(Which: Integer; aPage: Integer);
      procedure ScrollBarShow(Which: Integer; aValue: boolean);
      procedure ScrollToFocus;
      procedure UpdateHorzScrollBar(const aVisible: boolean; const aRange,aPage,aPos: Integer);
      procedure UpdateVertScrollbar(const aVisible: boolean; const aRange,aPage,aPos: Integer);
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
      FDateFormat: String;
      FEndDate: TDate;
      FFactIntervalColor: TColor;
      FOnClick: TNotifyEvent;
      FOnDblClick: TNotifyEvent;
      FPlanIntervalColor: TColor;
      FFreeIntervalColor: TColor;
      FExpiredIntervalColor: TColor;
      FStartDate: TDate;
      FvpGanttOptions: TvpGanttOptions;
      FvpGanttTasks: TvpGanttTasks;
      FvpGanttCalendar: TvpGanttCalendar;
      FSplitter: TSplitter;
      FScrollBars: TScrollStyle;
      FVScrollPosition: integer;
      FScrollBarHeight: integer;
      FScrollBarWidth: integer;
      FFocusRect: TRect;
      FMouseInterval: integer;
      FSavedHint: string;

      FIntervals: TList;
      FStartDateOfBound: TDateTime;
      FEndDateOfBound: TDateTime;

      FUpdateCount: integer;

      FRowHeight: integer;
      FMajorScaleHeight: integer;
      FMinorScaleHeight: integer;
      FIntervalsHeight: integer;
      FGridBorderWidth: integer;
      FBorderColor: TColor;
      FFocusColor: TColor;
      FRowHighlightColor: TColor;
      FTitleColor: TColor;
      FTaskTitleCaption: TCaption;
      FTitleFont: TFont;
      FTitleFontIsDefault: boolean;
      FTitleStyle: TTitleStyle;
      FTitleTextStyle: TTextStyle;
      FFocusRow: integer;
      //methods
      procedure ClearBoundDates;
      function GetCalendarColor: TColor;
      function GetIntervalCount: Integer;
      function GetInterval(AnIndex: Integer): TvpInterval;
      function GetBorderWidth: integer;
      function GetHorzBorderWidth: integer;
      function GetVertBorderWidth: integer;
      function GetSelectedIndex: integer;
      function GetMajorScale: TvpTimeScale;
      function GetMinorScale: TvpTimeScale;
      function GetPixelPerMinorScale: integer;
      function GetTaskColor: TColor;
      function GetTaskTitleCaption: TCaption;
      procedure OnTitleFontChanged(Sender: TObject);
      function OptionsIsStored: Boolean;
      procedure SetDateFormat(AValue: String);
      procedure SetEndDate(AValue: TDate);
      procedure SetExpiredIntervalColor(AValue: TColor);
      procedure SetFactIntervalColor(AValue: TColor);
      procedure SetFocusColor(AValue: TColor);
      procedure SetFreeIntervalColor(AValue: TColor);
      procedure SetMajorScale(AValue: TvpTimeScale);
      procedure SetMajorScaleHeight(AValue: integer);
      procedure SetMinorScale(AValue: TvpTimeScale);
      procedure SetMinorScaleHeight(AValue: integer);
      procedure SetOptions(AValue: TvpGanttOptions);
      procedure SetPixelPerMinorScale(AValue: integer);
      procedure SetPlanIntervalColor(AValue: TColor);
      procedure SetRowHeight(AValue: integer);
      procedure SetGridBorderWidth(AValue: integer);
      procedure SetBorderColor(AValue: TColor);
      procedure SetStartDate(AValue: TDate);
      procedure SetTaskColor(AValue: TColor);
      procedure SetCalendarColor(AValue: TColor);
      procedure SetScrollBars(const AValue: TScrollStyle);
      procedure SetTaskTitleCaption(AValue: TCaption);
      procedure SetTitleColor(AValue: TColor);
      procedure SetTitleFont(const AValue: TFont);
      procedure SetTitleStyle(const AValue: TTitleStyle);
      procedure UpdateSizes;
      procedure WMSize(var message: TLMSize); message LM_SIZE;
      procedure WMKillFocus(var message: TLMKillFocus); message LM_KILLFOCUS;
      procedure WMSetFocus(var message: TLMSetFocus); message LM_SETFOCUS;
    protected
      procedure CalcIntervalsHeight;
      procedure CalcFocusRect;
      procedure CutBorderFromRect(var aRect: TRect);
      procedure CutHBorderFromRect(var aRect: TRect);
      procedure CutVBorderFromRect(var aRect: TRect);
      procedure DoClick(Sender: TObject);
      procedure DoDblClick(Sender: TObject);
      procedure DrawCellGrid(ACanvas: TCanvas; const aRect: TRect);
      procedure DrawCell(aRow: integer; ACanvas: TCanvas; const aRect: TRect);
      procedure DrawFocusRect(ACanvas: TCanvas; const aRect: TRect);
      procedure DrawFillRect(ACanvas:TCanvas; const aRect: TRect); deprecated;
      procedure DrawHighlightRect(ACanvas: TCanvas; const aRect: TRect);
      procedure DrawThemedCell(ACanvas: TCanvas; const aRect: TRect);
      procedure DrawTitleGrid(ACanvas: TCanvas; const aRect: TRect);
      procedure DrawTitleCell(ACanvas: TCanvas; const aRect: TRect);
      procedure DrawTitleText(ACanvas: TCanvas; const aRect: TRect; aText: string; aAligment: TAlignment = taLeftJustify);
      procedure FontChanged(Sender: TObject); override;
      function GetIntervalsHeight: integer; //высота всех интервалов
      function GetFocusRow: integer;
      function GetMajorScaleHeight: integer;
      function GetMinorScaleHeight: integer;
      function GetRowPosY(const YPos: integer): integer;
      function GetTitleHeight: integer;
      procedure HideRowHintWindow;
      procedure InvalidateFocused;
      procedure InvalidateRow(aRow: integer);
      procedure KeyDown(var Key : Word; Shift : TShiftState); override;
      procedure KeyUp(var Key : Word; Shift : TShiftState); override;
      procedure KeyPress(var Key: char); override;
      procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
      procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
      procedure Paint; override;
      function  ScrollBarAutomatic(Which: TScrollStyle): boolean;
      procedure SetFocusRow(AValue: integer);
      procedure SelectNextRow(const ADelta: integer);
      procedure ShowRowHintWindow(APoint: TPoint);
      procedure UpdateBoundDates(AStartDate, AEndDate: TDate);
      procedure VisualChange;
      //messages event
      procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message :TLMessage); message CM_MouseLeave;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      //property
      property Interval[Index: Integer]: TvpInterval read GetInterval;
      property SelectedIndex: integer read GetSelectedIndex;
      property IntervalCount: Integer read GetIntervalCount;
      procedure AddInterval(AnInterval: TvpInterval);
      procedure InsertInterval(AnIndex: Integer; AnInterval: TvpInterval);
      procedure DeleteInterval(AnIndex: Integer);
      procedure RemoveInterval(AnInterval: TvpInterval);
      procedure UpdateInterval(AnIndex: Integer = -1);

      procedure Clear;
      function GetStartDateOfBound: TDateTime;
      function GetEndDateOfBound: TDateTime;
      procedure RecalcIntervals;
      procedure UpdateDates;
      function Focused: boolean; override;

      procedure BeginUpdate;
      procedure EndUpdate(aRefresh: boolean = true);

      procedure First;
      procedure Last;
      procedure MoveTo(ARow: integer);
      procedure SetFocus; override;
      property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;
    published
      //standart TwinControl property
      property Align;
      property BorderStyle;
      property BorderWidth;
      //events
      property OnClick;
      property OnDblClick;
      property OnResize;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      //custom property
      property BorderColor: TColor read FBorderColor write SetBorderColor default clActiveBorder;
      property EndDate: TDate read FEndDate write SetEndDate;
      property DateFormat: String read FDateFormat write SetDateFormat;
      property GridBorderWidth: Integer read FGridBorderWidth write SetGridBorderWidth default 1;
      property MajorScale: TvpTimeScale read GetMajorScale write SetMajorScale default vptsMonth;
      property MajorScaleHeight: integer read FMajorScaleHeight write SetMajorScaleHeight default C_DEF_ROW_HEIGHT;
      property MinorScale: TvpTimeScale read GetMinorScale write SetMinorScale default vptsDay;
      property MinorScaleHeight: integer read FMinorScaleHeight write SetMinorScaleHeight default C_DEF_ROW_HEIGHT;
      property Options: TvpGanttOptions read FvpGanttOptions write SetOptions stored OptionsIsStored default DefaultGanttOptions;
      property PixelPerMinorScale: integer read GetPixelPerMinorScale write SetPixelPerMinorScale default C_DEF_PIXEL_PER_MINOR;
      property RowHeight: integer read FRowHeight write SetRowHeight default C_DEF_ROW_HEIGHT;
      property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssAutoBoth;
      property StartDate: TDate read FStartDate write SetStartDate;
      property TitleFont: TFont read FTitleFont write SetTitleFont;
      property TitleStyle: TTitleStyle read FTitleStyle write SetTitleStyle;
      //TODO: Move this properties to child control OR stay this HOW RULES?
      property CalendarColor:TColor read GetCalendarColor write SetCalendarColor default clWindow;
      property TaskColor: TColor read GetTaskColor write SetTaskColor default clWindow;
      property TitleColor: TColor read FTitleColor write SetTitleColor default clBtnFace;
      property TaskTitleCaption: TCaption read GetTaskTitleCaption write SetTaskTitleCaption;
      property PlanIntervalColor: TColor read FPlanIntervalColor write SetPlanIntervalColor default clAqua;
      property FactIntervalColor: TColor read FFactIntervalColor write SetFactIntervalColor;
      property FreeIntervalColor: TColor read FFreeIntervalColor write SetFreeIntervalColor default clLime;
      property ExpiredIntervalColor: TColor read FExpiredIntervalColor write SetExpiredIntervalColor default clRed;
  end;

  //draw
  procedure DrawRubberRect(Canvas: TCanvas; aRect: TRect; Color: TColor);
  function  GetWorkingCanvas(const Canvas: TCanvas): TCanvas;
  procedure FreeWorkingCanvas(canvas: TCanvas);
  //timescale
  function UnitsBetweenDates(Start, Finish: TdateTime; TimeScale: TvpTimeScale): Double;
  function UnitsBetweenDatesEx(Start, Finish: TdateTime; TimeScale: TvpTimeScale): Double;
  function GetTimeScaleName(TimeScale: TvpTimeScale; D: TDateTime): String;
  function IncTime(D: TDateTime; TimeScale: TvpTimeScale; IncAmount: Integer): TDateTime;
  function IncTimeEx(D: TDateTime; TimeScale: TvpTimeScale; IncAmount: Double): TDateTime;
  function DateInPeriod(const CurDate, StartPeriod, EndPeriod: TDateTime): boolean;

implementation

{$if Defined(DBGGANTT) OR Defined(DBGGANTTTASKS) OR Defined(DBGGANTTCALENDAR)}
uses Unit1;

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
{$endif}

{ Процедура прорисовки квадратика активного фокуса в окне.
  Взято из модуля Grids, почему не используется функция WinAPI DrawFocusRect могу только догадыватся.
  И это скорее всего связано с кроссплатформенностью, данный компонент ее не поддерживает, надеюсь пока.
  А функция уже реализована, потому будем юзать её.
  @param Canvas TCanvas канва на которой рисуем
  @param aRect TRect область фокуса для отрисовки
  @color TColor цвет прямоугольника
}
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

{ Получаем рабочий канвас, если он не существует или =0 то получаем указатель
  на рабочий стол
  @param Canvas TCanvas нужная канва
  @return TCanvas указатель на канву или рабочий стол
}
function GetWorkingCanvas(const Canvas: TCanvas): TCanvas;
var
  DC: HDC;
begin
  if (Canvas=nil) or (not Canvas.HandleAllocated) then
    begin
      DC := GetDC(0);
      Result := TCanvas.Create;
      Result.Handle := DC;
    end
  else
    Result := Canvas;
end;

{ Освобождаем рабочую канву
  @param Canvas TCanvas нужная канва
}
procedure FreeWorkingCanvas(canvas: TCanvas);
begin
  ReleaseDC(0, Canvas.Handle);
  Canvas.Free;
end;

{ Функция определения количества временных интервалов между двумя датами
  @param Start TDateTime начальное время
  @param Finish TDateTime конечное время
  @param TimeScale TvpTimeScale тип временной шкалы см. TvpTimeScale
}
function UnitsBetweenDates(Start, Finish: TDateTime; TimeScale: TvpTimeScale): Double;
begin
  case TimeScale of
    vptsMinute:
      Result := MinuteSpan(Start, Finish);
    vptsDecMinute:
      Result := MinuteSpan(Start, Finish)/10;
    vptsHour:
      Result := HourSpan(Start, Finish);
    vptsDay, vptsDayWeek:
      Result := DaySpan(Start, Finish);
    vptsWeek, vptsWeekNum, vptsWeekNumPlain:
      Result := WeekSpan(Start, Finish);
    vptsMonth:
      Result := MonthSpan(Start, Finish);
    vptsQuarter:
      Result := MonthSpan(Start, Finish)/3;
    vptsHalfYear:
      Result := MonthSpan(Start, Finish)/6;
    vptsYear:
      Result := YearSpan(Start, Finish);
    else
      Result := DaySpan(Start, Finish);
  end;
end;

{ Функция определения количества временных интервалов между двумя датами с
  использованием приведения времени к юниксовскому формату
  @param Start TDateTime начальное время
  @param Finish TDateTime конечное время
  @param TimeScale TvpTimeScale тип временной шкалы см. TvpTimeScale
}
function UnitsBetweenDatesEx(Start, Finish: TDateTime; TimeScale: TvpTimeScale): Double;
var
  StartStamp, FinishStamp: TTimeStamp;
  StartDay, StartMonth, StartYear: Word;
  FinishDay, FinishMonth, FinishYear: Word;
begin
  StartStamp := DateTimeToTimeStamp(Start);
  FinishStamp := DateTimeToTimeStamp(Finish);

  DecodeDate(Start, StartYear, StartMonth, StartDay);
  DecodeDate(Finish, FinishYear, FinishMonth, FinishDay);

  case TimeScale of
    vptsMinute:
      begin
        Result :=
          (FinishStamp.Time / 1000 / 60 + FinishStamp.Date * 24 * 60) -
          (StartStamp.Time / 1000 / 60 + StartStamp.Date * 24 * 60);
      end;
    vptsDecMinute:
      begin
        Result :=
          ((FinishStamp.Time / 1000 / 60  + FinishStamp.Date * 24 * 60) -
          (StartStamp.Time / 1000 / 600  + StartStamp.Date * 24 * 60)) / 10;
      end;
    vptsHour:
      begin
        Result :=
          (FinishStamp.Time / 1000 / 60 / 60 + FinishStamp.Date * 24) -
          (StartStamp.Time / 1000 / 60 / 60 + StartStamp.Date * 24);
      end;
    vptsDay, vptsDayWeek:
      begin
        Result :=
          (FinishStamp.Time / 1000 / 60 / 60 / 24 + FinishStamp.Date) -
          (StartStamp.Time / 1000 / 60 / 60 / 24 + StartStamp.Date);
      end;
    vptsWeek,vptsWeekNum,vptsWeekNumPlain:
      begin
        Result :=
          (FinishStamp.Time / 1000 / 60 / 60 / 24 / 7 + FinishStamp.Date / 7) -
          (StartStamp.Time / 1000 / 60 / 60 / 24 / 7 + StartStamp.Date / 7);
      end;
    vptsMonth:
      begin
        Result :=
          (FinishMonth +
            (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
            -
          (StartMonth +
            (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
            +
          (FinishYear - StartYear) * 12;
      end;
    vptsQuarter:
      begin
        Result :=
          (
            (FinishMonth +
              (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
            )
              -
            (StartMonth +
              (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
            )
          ) / 3
            +
          (FinishYear - StartYear) * 3;
      end;
    vptsHalfYear:
      begin
        Result :=
          (
            (FinishMonth +
              (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
            )
              -
            (StartMonth +
              (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
            )
          ) / 6
            +
          (FinishYear - StartYear) * 6;
      end;
    vptsYear:
      begin
        Result :=
          (
            (FinishMonth +
              (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
            )
              -
            (StartMonth +
              (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24) / MonthDays[IsLeapYear(FinishYear)][FinishMonth]
            ) / 12
          )
            +
          (FinishYear - StartYear);
      end;
    else
      begin
        Result :=
          FinishStamp.Time / 1000 + FinishStamp.Date * 24 * 60 * 60 -
          StartStamp.Time / 1000 + StartStamp.Date * 24 * 60 * 60;
      end;
  end;
end;

{ Функция возвращает название временной шкалы
  @param TimeScale TvpTimeScale тип временной шкалы
  @param D TDateTime время
  @return string название временного интервала (Пн. Вт, Январь, Ферваль, и т.д.)
}
function GetTimeScaleName(TimeScale: TvpTimeScale; D: TDateTime): String;
var
  Hour, Min, Sec, MSec: Word;
  Year, Month, Day: Word;
begin
  DecodeDate(D, Year, Month, Day);
  DecodeTime(D, Hour, Min, Sec, MSec);

  case TimeScale of
    vptsMinute:
      Result := IntToStr(Min);
    vptsDecMinute:
      Result := IntToStr(Min);
    vptsHour:
      Result := IntToStr(Hour) +  ' ' + RS_HOUR;
    vptsDay:
      Result := IntToStr(Day);
    vptsDayWeek:
      Result := DefaultFormatSettings.ShortDayNames[DayOfWeek(D)];
    vptsWeek:
      Result := IntToStr(Day) + '.' + IntToStr(Month);
    vptsWeekNum:
      Result := RS_WEEK + IntToStr(WeekOfTheYear(D));
    vptsWeekNumPlain:
      Result := IntToStr(WeekOfTheYear(D));
    vptsMonth:
      Result := DefaultFormatSettings.LongMonthNames[Month] + ' ' + IntToStr(Year);
    vptsQuarter:
      Result := RS_KW + IntToStr((Month) div 3 + 1);
    vptsHalfYear:
      Result := IntToStr((Month) div 6 + 1);
    vptsYear:
      Result := IntToStr(Year);
  end;
end;

{ Функция сдвига времени на нужный интервал. Будеи использовать для получения даты
  и передачи ее в функцию получения названия текущего периода.
  Например, стартовая дата 01.01.2000 года, диапазон старшей шкалы месяцы, значит первая дата
  будет 01.01.2000 для нее получим название "Январь", для получения следующей даты
  надо передать в функцию текущую дату и на сколько сдвигаем IncAmount. Сдвинем на 1 месяй и получим дату
  01.02.2000 прочитаем его название и получим "Февраль"
  @param D TDateTime текущее время
  @param TimeScale TvpTimeScale шкала времени
  @param IncAmount integer число деленний для сдвига
  @return TDateTime полученное после сдвига время
}
function IncTime(D: TDateTime; TimeScale: TvpTimeScale; IncAmount: Integer): TDateTime;
begin
  Result := D;
  if IncAmount=0 then
    Exit;
  case TimeScale of
    vptsMinute:
      Result := IncMinute(D, IncAmount);
    vptsDecMinute:
      Result := IncMinute(D, IncAmount * 10);
    vptsHour:
      Result := IncHour(D, IncAmount);
    vptsDay, vptsDayWeek:
      Result := IncDay(D, IncAmount);
    vptsWeek, vptsWeekNum, vptsWeekNumPlain:
      Result := IncWeek(D, IncAmount);
    vptsMonth:
      Result := IncMonth(D, IncAmount);
    vptsQuarter:
      Result := IncMonth(D, IncAmount * 3);
    vptsHalfYear:
      Result := IncMonth(D, IncAmount * 6);
    vptsYear:
      Result := IncYear(D, IncAmount);
  end;
end;

function IncTimeEx(D: TDateTime; TimeScale: TvpTimeScale; IncAmount: Double): TDateTime;
var
  S: TTimeStamp;
  Year, Month, Day: Word;
begin
  S := DateTimeToTimeStamp(D);

  case TimeScale of
    vptsMinute:
    begin
      if IncAmount > 24 * 60 then
      begin
        Inc(S.Date, Trunc(IncAmount / 24 * 60));
        IncAmount := IncAmount - Trunc(IncAmount / 24 * 60) * (24 * 60);
      end;

      Inc(S.Time, Trunc(IncAmount * 60 * 1000));

      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
    vptsHour:
    begin
      if IncAmount > 24 then
      begin
        Inc(S.Date, Trunc(IncAmount / 24));
        IncAmount := IncAmount - Trunc(IncAmount / 24) * 24;
      end;

      Inc(S.Time, Trunc(IncAmount * 60 * 60 * 1000));

      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
    vptsDay, vptsDayWeek:
    begin
      Inc(S.Date, Trunc(IncAmount));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), vptsHour, Frac(IncAmount) * 24
          )
        );
    end;
    vptsWeek,vptsWeekNum,vptsWeekNumPlain:
    begin
      Inc(S.Date, Trunc(IncAmount) * 7);

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), vptsDay, Frac(IncAmount) * 7
          )
        );
    end;
    vptsMonth:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount)));
      DecodeDate(TimeStampToDateTime(S), Year, Month, Day);

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), vptsDay,
            Frac(IncAmount) * MonthDays[IsLeapYear(Year)][Month]
          )
        );
    end;
    vptsQuarter:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount) * 3));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), vptsMonth, Frac(IncAmount) * 3
          )
        );
    end;
    vptsHalfYear:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount) * 6));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), vptsMonth, Frac(IncAmount) * 6
          )
        );
    end;
    vptsYear:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount) * 12));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), vptsMonth, Frac(IncAmount) * 12
          )
        );
    end;
    else begin
      if IncAmount > 24 * 60 * 60 then
      begin
        Inc(S.Date, Trunc(IncAmount / 24 * 60 * 60));
        IncAmount := IncAmount - IncAmount / 24 * 60 * 60;
      end;

      Inc(S.Time, Trunc(IncAmount * 1000));
      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
  end;

  Result := TimeStampToDateTime(S);
end;

{ Функция определения вхождения даты в заданный период
  @param CurDate TDateTime текущее время
  @param StartPeriod TDateTime начало периода
  @param EndPeriod TDateTime конец периода
  @return boolean истина/ложь
}
function DateInPeriod(const CurDate, StartPeriod, EndPeriod: TDateTime): boolean;
begin
  Result := (CurDate>=StartPeriod) AND (CurDate<=EndPeriod);
end;


{ TvpInterval }

procedure TvpInterval.SetStartDate(AValue: TDateTime);
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.SetStartDate');
  {$endif}
  if FStartDate = AValue then Exit;
  if (FFinishDate<>0) AND (AValue>FFinishDate) then
    Raise Exception.Create(RS_E_STARTDATE_HIGH);
  FStartDate := AValue;
  //расчитываем области
  CalcPlanBound;
end;

procedure TvpInterval.SetBoundHeight(const ATop, ABottom: integer);
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.SetFinishDate');
  {$endif}
  FPlanRect.Top := ATop;
  FPlanRect.Bottom := ABottom;
  //отсекаем лишнее по высоте
  InflateRect(FPlanRect, 0, -C_DEF_INTERVAL_PADDING);
  //чтобы не было обратной области
  FPlanRect.Bottom := Max(FPlanRect.Top, FPlanRect.Bottom);
  //применяем координаты высоты и для фактической области
  FFactRect.Top := FPlanRect.Top;
  FFactRect.Bottom := FPlanRect.Bottom;
end;

procedure TvpInterval.SetFinishDate(AValue: TDateTime);
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.SetFinishDate');
  {$endif}
  if FFinishDate = AValue then
    Exit;
  if AValue<FStartDate then
    Raise Exception.Create(RS_E_FINISHDATE_LESS);
  FFinishDate := AValue;
  //расчитываем области
  CalcFactBound;
end;

procedure TvpInterval.SetDuration(AValue: Double);
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.SetDuration');
  {$endif}
  if FDuration = AValue then
    Exit;
  FDuration := AValue;
  //расчитываем области
  CalcPlanBound;
end;

function TvpInterval.GetFinishDate: TDateTime;
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.GetFinishDate');
  {$endif}
  if FFinishDate=0 then
    Result := FStartDate + FDuration
  else
    Result := FFinishDate;
end;

function TvpInterval.GetComplete: Double;
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.GetFinishDate');
  {$endif}
  if FFinishDate=0 then
    Result := 0
  else
    Result := FFinishDate - FStartDate;
end;

{ Процедура расчета области планируемой длительности интервала
  Рассчитываем левую границу всегда. А вот правую считаем только если задана положительная
  продолжительность
}
procedure TvpInterval.CalcPlanBound;
var
  countTimeScale: double;
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.CalcLeftPlanBound');
  {$endif}
  countTimeScale := UnitsBetweenDates(FvpGantt.GetStartDateOfBound, FStartDate, FvpGantt.MinorScale);
  FPlanRect.Left := Trunc(countTimeScale * FvpGantt.PixelPerMinorScale);
  if FDuration>0 then
    begin
      countTimeScale := UnitsBetweenDates(FStartDate, FStartDate + FDuration, FvpGantt.MinorScale);
      FPlanRect.Width := Trunc(countTimeScale * FvpGantt.PixelPerMinorScale);
    end
  else
    FPlanRect.Width := 0;
  {$ifdef DBGINTERVAL}
  Form1.EL.Debug('FPlanRect LTRB %d %d %d %d', [FPlanRect.Left, FPlanRect.Top, FPlanRect.Right, FPlanRect.Bottom]);
  {$endif}
end;

{ Процедура расчета области фактической длительности интервала
  Рассчитываем только в случае, если задана дата окончания интервала и она больше даты начала
}
procedure TvpInterval.CalcFactBound;
var
  countTimeScale: Double;
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.CalcRightDueBound');
  {$endif}
  if FFinishDate>FStartDate then
    begin
      FFactRect.Left := FPlanRect.Left;
      countTimeScale := UnitsBetweenDates(FStartDate, FFinishDate, FvpGantt.MinorScale);
      FFactRect.Width := Trunc(countTimeScale * FvpGantt.PixelPerMinorScale);
    end;
  if FDuration=0 then
    FPlanRect.Right := FFactRect.Right;
  {$ifdef DBGINTERVAL}
  Form1.EL.Debug('FDurationRect LTRB %d %d %d %d', [FDurationRect.Left, FDurationRect.Top, FDurationRect.Right, FDurationRect.Bottom]);
  {$endif}
end;

{ Конструктор
}
constructor TvpInterval.Create(AvpGantt: TvpGantt);
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.Create');
  {$endif}
  FvpGantt := AvpGantt;
  //initialize
  FPlanRect := Rect(0, 0, 0, 0);
  FFactRect := Rect(0, 0, 0, 0);
  //dates
  FStartDate := 0;
  FFinishDate := 0;
  FDuration := 0;
end;

{ Разрушитель :)
}
destructor TvpInterval.Destroy;
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.Destroy');
  {$endif}
  inherited Destroy;
end;

function TvpInterval.IsExpired: boolean;
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.IsExpired');
  {$endif}
  Result := FFinishDate > (FStartDate + FDuration);
end;

{ Процедура пересчета границ интервала. Если сделаны изменения шкал, дат и кол-ва интервало
  то следует вызвать для каждого интервала данную процедуру, чтобы не тратить время на расчет при
  прорисовке календаря
}
procedure TvpInterval.UpdateBounds;
begin
  {$ifdef DBGINTERVAL}
  Form1.Debug('TvpInterval.UpdateBounds');
  {$endif}
  CalcPlanBound;
  CalcFactBound;
end;

{ TvpGanttCalendar }

procedure TvpGanttCalendar.ScrollBarShow(Which: Integer; aValue: boolean);
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ScrollBarShow');
  {$endif}
  if HandleAllocated then begin
    ShowScrollBar(Handle,Which,aValue);
    if Which in [SB_BOTH, SB_VERT] then FVSbVisible := AValue else
    if Which in [SB_BOTH, SB_HORZ] then FHSbVisible := AValue;
  end;
end;

procedure TvpGanttCalendar.ScrollToFocus;
var
  oldPos, curPos: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ScrollToFocusRect');
  {$EndIf}
  curPos := GetScrollPos(Handle, SB_VERT);
  oldPos := curPos;
  //если нижняя граница области рисования фокуса ниже низа клиентской области
  //то прокручиваем на разницу между грfницами фокуса
  if TvpGantt(Parent).FFocusRect.Bottom>ClientHeight then
    begin
      curPos := curPos + TvpGantt(Parent).FFocusRect.Bottom - ClientRect.Bottom;
      //не должно быть > максимальногго диспазона прокрутки диапазон скрола - высота страницы скрола - 1
      curPos := Min(curPos, FCalendarHeight - ClientHeight);
    end
  //иначе если верхняя граница области выделения выше видимой области сетка,
  //то прокручиваем ширину строки
  else if TvpGantt(Parent).FFocusRect.Top<FGridVisibleRect.Top then
    begin
      curPos := curPos + TvpGantt(Parent).FFocusRect.Top - FGridVisibleRect.Top;
      //не должно быть отрицательным, т.е. если сдвинуты на пол строки и дошли до первой, то крутатнуть надо до 0,
      //а никак не в отрицательную позицию
      curPos := Max(0, curPos);
    end;
  //если позиция ползунка изменилась, то перерисовываем все
  //иначе перерисовываем только фокус
  if curPos<>oldPos then
    begin
      ScrollBarPosition(SB_VERT, curPos);
      TvpGantt(Parent).FVScrollPosition := curPos;
      TvpGantt(Parent).Invalidate;
    end
  else
    TvpGantt(Parent).InvalidateFocused;
  //убираем сообщение, т.к. в этом случае мы не можем переслать позицию указателя выше 65535 :(
  //SendMessage(Handle, LM_VSCROLL, MakeWParam(SB_THUMBPOSITION, curPos), 0);
  {$ifdef DBGGANTTCALENDAR}
  Form1.El.Debug('FFocusRow %d  curPos %d  FFocusRect.Bottom %d ClientRect.Bottom %d FFocusRect.Top %d ClientRect.Top - FvpGantt.GetTitleHeight %d',
                 [FvpGantt.FFocusRow, curPos, FvpGantt.FFocusRect.Bottom, ClientRect.Bottom, FvpGantt.FFocusRect.Top, ClientRect.Top - FvpGantt.GetTitleHeight]);
  {$EndIf}
end;

function TvpGanttCalendar.CalcRowRect(const aRow: integer): TRect;
var
  aStart: integer;
  tmpRect: TRect;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcRowRect');
  {$EndIf}
  Result := TRect.Create(0,0,0,0);
  aStart := TvpGantt(Parent).GetTitleHeight +
            TvpGantt(Parent).RowHeight * aRow - TvpGantt(Parent).FVScrollPosition;
  //находим всю длину строки
  tmpRect := Rect(0 - FHScrollPosition, aStart,
                 FCalendarWidth - FHScrollPosition, aStart + TvpGantt(Parent).RowHeight);
  //обрезаем невидимые части
  {DONE -o Vas Пофиксить обрезку части строки внизу, иначе выводится черти-что}
  IntersectRect(Result, tmpRect, FGridVisibleRect);
end;

procedure TvpGanttCalendar.CalcScaleCount;
var
  i: integer;
  aSDate, aEDate: TDateTime;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcScale');
  {$endif}
  if (TvpGantt(Parent).FStartDateOfBound=0) OR (TvpGantt(Parent).FEndDateOfBound=0) then
    Exit;
  FMajorScaleCount := Round(UnitsBetweenDates(TvpGantt(Parent).FStartDateOfBound, TvpGantt(Parent).FEndDateOfBound, FMajorScale));
  FMinorScaleCount := Round(UnitsBetweenDates(TvpGantt(Parent).FStartDateOfBound, TvpGantt(Parent).FEndDateOfBound, FMinorScale));
  //заполняем массив
  SetLength(FMinorCountInMajor, FMajorScaleCount);
  //считаем кол-во меньших диапазонов в большем, если меньший < vptsMonth, то кол-во диапазонов в нем будет разное
  //т.к. в месяце раное кол-во дней, соответсвено и разное все что ниже месяца
  //иначе одинаковое, т.к. в году 2 полугодия, 4 кавартала, 12 месяцев и получаем все обычным делением
  if FMinorScale<vptsMonth then
    begin
      aSDate := TvpGantt(Parent).FStartDateOfBound;
      for i:=0 to FMajorScaleCount-1 do
        begin
          aEDate := IncTime(aSDate, FMajorScale, 1);
          FMinorCountInMajor[i] := Round(UnitsBetweenDates(aSDate, aEDate, FMinorScale));
          aSDate := aEDate;
        end;
    end
  else
    begin
      for i:=0 to FMajorScaleCount-1 do
        FMinorCountInMajor[i] := FMinorScaleCount div FMinorScaleCount;
    end;

  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('Major scale count %d', [FMajorScaleCount]);
  Form1.EL.Debug('Minor scale count %d', [FMinorScaleCount]);
  {$endif}
end;

procedure TvpGanttCalendar.CalcCalendarHeight;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcCalendarHeight');
  Form1.El.Debug('FvpGantt.IntervalCount=%d FvpGantt.GetIntervalsHeight=%d', [FvpGantt.IntervalCount, FvpGantt.GetIntervalsHeight]);
  {$endif}
  FCalendarHeight := TvpGantt(Parent).GetTitleHeight + TvpGantt(Parent).GetIntervalsHeight;
  {$ifdef DBGGANTTCALENDAR}
  Form1.El.Debug('FCalendarHeight %d', [FCalendarHeight]);
  {$endif}
end;

procedure TvpGanttCalendar.CalcCalendarWidth;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcCalendarWidth');
  {$endif}
  FCalendarWidth := FMinorScaleCount * FPixelePerMinorScale;
  {$ifdef DBGGANTTCALENDAR}
  Form1.El.Debug('FCalendarWidth %d', [FCalendarWidth]);
  {$endif}
end;

procedure TvpGanttCalendar.CalcGridVisibleRect;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcGridVisibleRect');
  {$endif}
  //рассчитываем область
  FGridVisibleRect := ClientRect;
  InflateRect(FGridVisibleRect, -1, 0);
  FGridVisibleRect.Top := ClientRect.Top + TvpGantt(Parent).GetTitleHeight;
  FGridVisibleRect.Width := Min(FCalendarWidth - FHScrollPosition, ClientWidth);
  FGridVisibleRect.Height := Ceil(ClientHeight / TvpGantt(Parent).RowHeight)* TvpGantt(Parent).RowHeight;
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('FGridVisibleRect Left %d Top %d Right %d Bottom %d',
                  [FGridVisibleRect.Left, FGridVisibleRect.Top, FGridVisibleRect.Right, FGridVisibleRect.Bottom]);
  {$ENDIF}
end;

function TvpGanttCalendar.CalcFocusRect: TRect;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcFocusRect');
  {$endif}
  Result := TvpGantt(Parent).FFocusRect;
  Result.Left := FGridVisibleRect.Left;
  Result.Right := Result.Right + FCalendarWidth;
  //срезаем невидимую часть области
  IntersectRect(Result, Result, FGridVisibleRect);
end;

function TvpGanttCalendar.GetMajorScaleHeight: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetMajorScaleHeight');
  {$endif}
  Result := FMajorScaleTitleRect.Height;
end;

function TvpGanttCalendar.GetMajorScaleWidth(const aCurItem: integer): integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetMajorScaleWidth');
  {$endif}
  if aCurItem>=FMajorScaleCount then
    Result := 0
  else
    Result := FPixelePerMinorScale * FMinorCountInMajor[aCurItem];
end;

function TvpGanttCalendar.GetMinorScaleHeight: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetMinorScaleHeight');
  {$endif}
  Result := FMinorScaleTitleRect.Height;
end;

function TvpGanttCalendar.GetMinorScaleWidth: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetMinorScaleWidth');
  {$endif}
  Result := FPixelePerMinorScale;
end;

procedure TvpGanttCalendar.UpdateSBVisibility;
var
  HSbVisible, VSbVisible: boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.UpdateSBVisibility');
  {$endif}
  GetSBVisibility(HSbVisible, VSbVisible);
  ScrollBarShow(SB_VERT, VSbVisible);
  ScrollBarShow(SB_HORZ, HSbVisible);
end;

procedure TvpGanttCalendar.UpdateSizes;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.UpdateSizes');
  {$endif}
  FGridTextHeight := Canvas.GetTextHeight('A');
  CalcCalendarHeight;
  CalcCalendarWidth;
  //сдвигаем правый край, только если сетка нарсиована не с первого диапазона
  //и ширина сетки - скролл горизонтальный < клиентской области
  //текущую позицию сдвига уменьшаем на разницу между шириной клиентской области и видимой области сетки
  if (FHScrollPosition>0) AND (FCalendarWidth-FHScrollPosition<ClientWidth) then
    begin
      FHScrollPosition := Max(0, FHScrollPosition - (ClientWidth - FGridVisibleRect.Width));
      {$ifdef DBGSCROLL}
      Form1.Debug('FCalendarWidth-FHScrollPosition<ClientWidth');
      Form1.EL.Debug('FHScrollPosition %d FCalendarWidth %d ClientWidth %d - FGridVisibleRect.Right %d ClientRect.Right %d',
                      [FHScrollPosition, FCalendarWidth, ClientWidth, FGridVisibleRect.Right, ClientRect.Right]);
      {$endif}
    end;
  //пересчитываем видимую область
  CalcScrollbarsRange;
  CalcGridVisibleRect;
end;

procedure TvpGanttCalendar.VisualChange;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('TvpGanttCalendar.VisualChange INIT %s',[DbgSName(Self)]);
  {$endif}
  UpdateSizes;
  Invalidate;
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('TvpGanttCalendar.VisualChange END %s',[DbgSName(Self)]);
  {$endif}
end;

procedure TvpGanttCalendar.WMHScroll(var message: TLMHScroll);
var
  aPos, maxPos: Int64;
  ScrollInfo: TScrollInfo;
  aCode: Smallint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.WMHScroll');
  {$endif}
  if not HandleAllocated then
    exit;

  if csDesigning in ComponentState then
    exit;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_POS;
  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  maxPos := ScrollInfo.nMax - Max(ScrollInfo.nPage-1, 0);

  aCode := message.ScrollCode;
  aPos := ScrollInfo.nPos;

  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug(Format('maxPos %d', [maxPos]));
  Form1.Debug(Format('aPos %d', [aPos]));
  {$endif}
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

  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug(Format('aPos %d', [aPos]));
  {$endif}

  ScrollBarPosition(SB_HORZ, aPos);
  FHScrollPosition := aPos;

  {$ifdef DBGGANTTCALENDAR}
  //Form1.Debug(Format('FScrollPosition %d', [FXScrollPosition]));
  {$endif}

  Invalidate;
end;

procedure TvpGanttCalendar.WMVScroll(var message: TLMVScroll);
var
  aPos, maxPos: Int64;
  ScrollInfo: TScrollInfo;
  aCode: Smallint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.WMVScroll');
  {$endif}
  if not HandleAllocated then
    exit;

  if csDesigning in ComponentState then
    exit;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_POS;
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  maxPos := ScrollInfo.nMax - Max(ScrollInfo.nPage-1, 0);

  aCode := message.ScrollCode;
  aPos := ScrollInfo.nPos;

  {$ifdef DBGSCROLL}
  Form1.Debug(Format('maxPos %d', [maxPos]));
  Form1.Debug(Format('aPos %d', [aPos]));
  {$endif}
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

  {$ifdef DBGSCROLL}
  Form1.Debug(Format('aPos %d', [aPos]));
  {$endif}

  ScrollBarPosition(SB_VERT, aPos);
  TvpGantt(Parent).FVScrollPosition := aPos;

  {$ifdef DBGSCROLL}
  //Form1.Debug(Format('FVScrollPosition %d', [FVScrollPosition]));
  {$endif}

  //FvpGantt.CalcFocusRect;
  TvpGantt(Parent).Repaint;
end;

procedure TvpGanttCalendar.WMSize(var Message: TLMSize);
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.WMSize');
  Form1.EL.Debug('IsResizing %d', [Integer(IsResizing)]);
  {$endif}
  if not IsResizing then
    begin
      UpdateSizes;
      inherited;
    end;
end;

procedure TvpGanttCalendar.CalcScrollbarsRange;
var
  HsbVisible, VsbVisible: boolean;
  HsbRange,VsbRange: Integer;
  HsbPage, VsbPage: Integer;
  HsbPos, VsbPos: Integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcScrollbarsRange');
  {$endif}
  GetSBVisibility(HsbVisible, VsbVisible);
  GetSBRanges(HsbVisible, VsbVisible, HsbRange, VsbRange, HsbPage, VsbPage, HsbPos, VsbPos);
  UpdateVertScrollBar(VsbVisible, VsbRange, VsbPage, VsbPos);
  UpdateHorzScrollBar(HsbVisible, HsbRange, HsbPage, HsbPos);
end;

procedure TvpGanttCalendar.ScrollBarRange(Which: Integer; aRange, aPage,
  aPos: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ScrollBarRange');
  {$endif}
  if HandleAllocated then begin
    {$ifdef DBGGANTTCALENDAR}
    Form1.EL.Debug('ScrollbarRange: Which=%s Range=%d Page=%d Pos=%d',
      [SbToStr(Which), aRange, aPage, aPos]);
    {$endif}
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_DISABLENOSCROLL;
    //if not (gfPainting in FGridFlags) then
    //  ScrollInfo.fMask := ScrollInfo.fMask or SIF_POS;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := aRange;
    ScrollInfo.nPos := aPos;
    if APage<0 then
      APage := 0;
    ScrollInfo.nPage := APage;
    SetScrollInfo(Handle, Which, ScrollInfo, True);
  end;
end;

procedure TvpGanttCalendar.ScrollBarPosition(Which, Value: integer);
var
  ScrollInfo: TScrollInfo;
  Vis: Boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ScrollBarPosition');
  {$endif}
  if HandleAllocated then begin
    {$ifdef DBGGANTTCALENDAR}
    Form1.EL.Debug('ScrollbarPosition: Which= %s Value= %d', [SbToStr(Which), Value]);
    {$endif}
    if Which = SB_VERT then Vis := FVSbVisible else
    if Which = SB_HORZ then Vis := FHSbVisible
    else vis := false;
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_POS;
    ScrollInfo.nPos:= Value;
    SetScrollInfo(Handle, Which, ScrollInfo, Vis);
  end;
end;

function TvpGanttCalendar.ScrollBarIsVisible(Which: Integer): Boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ScrollBarIsVisible');
  {$endif}
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
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ScrollBarPage');
  {$endif}
  if HandleAllocated then begin
    {$ifdef DBGGANTTCALENDAR}
    Form1.EL.Debug('ScrollbarPage: Which= %s Avalue=%s', [SbToStr(Which), dbgs(aPage)]);
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
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('TvpGanttCalendar.UpdateHorzScrollbar: Vis=%s Range=%d Page=%d aPos=%d',
    [dbgs(aVisible),aRange, aPage, aPos]);
  {$endif}
  ScrollBarShow(SB_HORZ, aVisible);
  if aVisible then
    ScrollBarRange(SB_HORZ, aRange, aPage, aPos);
end;

procedure TvpGanttCalendar.UpdateVertScrollbar(const aVisible: boolean;
  const aRange, aPage, aPos: Integer);
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('TvpGanttCalendar.UpdateVertScrollbar: Vis=%s Range=%d Page=%d aPos=%d',
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
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CreateParams');
  {$endif}
  inherited CreateParams(Params);
  with Params do begin
    WindowClass.Style := WindowClass.Style and DWORD(not ClassStylesOff);
    Style := Style or WS_VSCROLL or WS_HSCROLL or WS_CLIPCHILDREN;
  end;
end;

procedure TvpGanttCalendar.ClearCanvas;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ClearCanvas');
  {$endif}
  Canvas.Brush.Color := TvpGantt(Parent).CalendarColor;
  Canvas.FillRect(ClientRect);
end;


function TvpGanttCalendar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DoMouseWheel');
  Form1.ShowParam(IntToStr(WheelDelta));
  {$endif}
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TvpGanttCalendar.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DoMouseWheelDown');
  {$endif}
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    begin
      TvpGantt(Parent).SelectNextRow(1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

function TvpGanttCalendar.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DoMouseWheelUp');
  {$endif}
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    begin
      TvpGantt(Parent).SelectNextRow(-1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

procedure TvpGanttCalendar.DrawCurrentTimeLine;
var
  lineLeft, lineBottom: integer;
  timePadding: Double;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.');
  {$endif}
  if not (vpgShowCurrentTime in TvpGantt(Parent).Options) then
    Exit;
  if DateInPeriod(Now, TvpGantt(Parent).FStartDateOfBound, TvpGantt(Parent).FEndDateOfBound) then
    begin
      timePadding := UnitsBetweenDates(TvpGantt(Parent).FStartDateOfBound, Now, FMinorScale);
      lineLeft := Trunc(timePadding * TvpGantt(Parent).PixelPerMinorScale) - FHScrollPosition;
      //рисуем до конца видимой части компонента или до конца списка
      if vpgExtendVertLines in TvpGantt(Parent).Options then
        lineBottom := ClientHeight
      else
        lineBottom := FCalendarHeight;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := C_CURTIMELINE_WIDTH;
      Canvas.Pen.Color := C_CURTIMELINE_COLOR;
      Canvas.Line(lineLeft, TvpGantt(Parent).GetTitleHeight, lineLeft, lineBottom - 1);
    end;
end;

procedure TvpGanttCalendar.DrawEdges;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawEdges');
  {$endif}
  Canvas.Pen.Width := C_DEF_BORDER_WIDTH;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clActiveBorder;
  Canvas.MoveTo(0, TvpGantt(Parent).GetTitleHeight);
  Canvas.LineTo(0, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right - 1, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right - 1, TvpGantt(Parent).GetTitleHeight);
end;


procedure TvpGanttCalendar.DrawMajorScale;
var
  aRect: TRect;
  aScaleName: string;
  i: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawMajorScale');
  {$endif}
  //берем нулевую область для старта
  aRect := Rect(0, 0, 0, TvpGantt(Parent).MajorScaleHeight);
  aRect.Offset( - FHScrollPosition, 0);
  //шрифт
  if not TvpGantt(Parent).FTitleFontIsDefault then
    Canvas.Font.Assign(TvpGantt(Parent).TitleFont)
  else
    Canvas.Font := Font;
  //перебираем все мажорные диапазоны
  for i:=0 to FMajorScaleCount-1 do
    begin
      aRect.Width := GetMajorScaleWidth(i);
      //если области пересекаются, то значит их рисовать
      if aRect.IntersectsWith(ClientRect) then
        begin
          aScaleName := GetTimeScaleName(FMajorScale, IncTime(TvpGantt(Parent).FStartDateOfBound, FMajorScale, i));
          //рисуем
          TvpGantt(Parent).DrawTitleCell(Canvas, aRect);
          TvpGantt(Parent).DrawTitleText(Canvas, aRect, aScaleName, taLeftJustify);
        end;
      //рисуем вертикальные разделители времени только если не рисовать от минорной области
      if not (vpgMinorVertLine in TvpGantt(Parent).Options) AND
             (vpgMajorVertLine in TvpGantt(Parent).Options) then
        DrawScaleVertLines(aRect.Right-1);
      //сдвигаем область на начало следующего диапазона
      aRect.Offset(aRect.Width, 0);
    end;
end;

procedure TvpGanttCalendar.DrawMinorScale;
var
  aRect: TRect;
  i: integer;
  aScaleName: string;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawMinorScale');
  {$endif}
  //берем нулевую область для старта
  aRect := Rect(0, TvpGantt(Parent).MajorScaleHeight,
                GetMinorScaleWidth, TvpGantt(Parent).MajorScaleHeight + TvpGantt(Parent).MinorScaleHeight);
  aRect.Offset( - FHScrollPosition, 0);
  //шрифт
  if not TvpGantt(Parent).FTitleFontIsDefault then
    Canvas.Font.Assign(TvpGantt(Parent).TitleFont)
  else
    Canvas.Font := Font;
  //перебираем все мажорные диапазоны
  for i:=0 to FMinorScaleCount-1 do
    begin
      //если области пересекаются, то значит их рисовать
      if aRect.IntersectsWith(ClientRect) then
        begin
          aScaleName := GetTimeScaleName(FMinorScale, IncTime(TvpGantt(Parent).FStartDateOfBound, FMinorScale, i));
          //рисуем
          TvpGantt(Parent).DrawTitleCell(Canvas, aRect);
          TvpGantt(Parent).DrawTitleText(Canvas, aRect, aScaleName, taCenter);
        end;
      //рисуем вертикальные разделители времени
      if vpgMinorVertLine in TvpGantt(Parent).Options then
        DrawScaleVertLines(aRect.Right-1);
      //сдвигаем область на начало следующего диапазона
      aRect.Offset(aRect.Width, 0);
    end;
end;

procedure TvpGanttCalendar.DrawInterval(const aRow: integer);
var
  drawRect, expRect: TRect;
  curInterval: TvpInterval;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawInterval');
  {$endif}
  curInterval := TvpGantt(Parent).Interval[aRow];
  //смещаем скролл
  drawRect := TRect.Create(curInterval.FPlanRect);
  OffsetRect(drawRect, -FHScrollPosition, 0);
  expRect := TRect.Create(curInterval.FFactRect);
  OffsetRect(expRect, -FHScrollPosition, 0);
  //кисть и перо
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := TvpGantt(Parent).PlanIntervalColor;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := cl3DDkShadow;
  //проверяем на завершение
  if curInterval.FFinishDate>0 then
    begin
      //рисуем основную заливку
      Canvas.Brush.Color := TvpGantt(Parent).FactIntervalColor;
      Canvas.RoundRect(drawRect, C_DEF_INTERVAL_RADIUS, C_DEF_INTERVAL_RADIUS);
      //если интервал вышел за пределы
      if curInterval.IsExpired then
        begin
          Canvas.Brush.Color := TvpGantt(Parent).ExpiredIntervalColor;
          //сдвигаем на 1 чтобы нормально отрисовалась общая область
          expRect.Left := drawRect.Right-1;
        end
      else
        begin
          Canvas.Brush.Color := TvpGantt(Parent).FreeIntervalColor;
          expRect.Left := expRect.Right-1;
          expRect.Right := drawRect.Right;
        end;
      //рисуем область первышение или свободного времени,
      //только если ширина расширенной части > 2 пикселей, иначе нарисуется только обводка
      if expRect.Width>2 then
        begin
          Canvas.RoundRect(expRect, C_DEF_INTERVAL_RADIUS, C_DEF_INTERVAL_RADIUS);
          Canvas.MoveTo(expRect.Left, expRect.Top);
          Canvas.LineTo(expRect.Left, expRect.Bottom);
        end;
    end
  else
    Canvas.RoundRect(drawRect, C_DEF_INTERVAL_RADIUS, C_DEF_INTERVAL_RADIUS);
end;

{ Процедура прорисовки строки.
  Рисуется только видимые диапазаоны. Для этого сначала вычисляется область всей строки
  с учетом горизонтально прокрутки. И проверяется ее высота, если она меньше или равна нулю, то эта
  строка не попадает в область ClientRect компонента FvpGanttCalendar. Таким образом отрисовываем только
  строки из видимого диапазона.
  Аналогично рисуем диапазоны. Берем области с заданной шириной aMinorScaleWidth
  и смотрим, если диапазон попадает в видимую область FvpGanttCalendar, то
  отрисовываем его, иначе идем дальше.
  Так мы рисуем только размер канваса FvpGanttCalrndar, что очень быстро и не тупит скролл.
}
procedure TvpGanttCalendar.DrawRow(const aRow: integer);
var
  rowRect, cellRect, tmpRect: TRect;
  aMinorScaleWidth: integer;
  i: integer;
  Dv: boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawRow');
  {$endif}
  if not Assigned(Parent) then
    Exit;
  //область строки
  rowRect := CalcRowRect(aRow);
  aMinorScaleWidth := GetMinorScaleWidth;
  Dv := vpgVertLine in TvpGantt(Parent).Options;
  //шрифт
  Canvas.Font := Font;
  {$ifdef DBGSCROLL}
  Form1.EL.Debug('Calendar rRect.Left %d rRect.Top %d rRect.Right %d rRect.Bottom %d',
                  [rowRect.Left, rowRect.Top, rowRect.Right, rowRect.Bottom]);
  Form1.EL.Debug('FHScrollPosition %d, ClientWidth %d, FGridVisibleRect.Width %d, FCalendarWidth %d',
                  [FHScrollPosition, ClientWidth, FGridVisibleRect.Width, FCalendarWidth]);
  {$endif}
  if rowRect.Height>0 then
    begin
      {$ifdef DBGGANTTCALENDAR}
      Form1.EL.Debug('Drawing row %d', [aRow]);
      {$endif}
      //нужно нарисовать строку по ячейкам старшей или младшей шкалы
      for i:=0 to FMinorScaleCount - 1 do
        begin
          //устанавливаем размер области = размеру области строки
          cellRect := TRect.Create(rowRect);
          //вычисляем с учетом сдвига, ширина будет равна FPixelePerMinorScale;
          cellRect.Left := aMinorScaleWidth * i - FHScrollPosition;
          cellRect.Right := cellRect.Left + aMinorScaleWidth;
          {$ifdef DBGSCROLL}
          Form1.EL.Debug('Before offset i %d cRect.Left %d cRect.Top %d cRect.Right %d cRect.Bottom %d',
                          [i, cellRect.Left, cellRect.Top, cellRect.Right, cellRect.Bottom]);
          {$endif}
          //пересекаем с видимой область строки и сохраняем результат во временную область
          //для проверки. А рисовать придется в полном прямоугольнике ячейки
          IntersectRect(tmpRect, cellRect, rowRect);
          //если ширина ячейки > 0 то рисуем
          if tmpRect.Width>0 then
            begin
              TvpGantt(Parent).DrawCellGrid(Canvas, cellRect);
              Canvas.Brush.Color := FColor;
              if Dv AND ([vpgMajorVertLine, vpgMinorVertLine]*TvpGantt(Parent).Options=[]) then
                TvpGantt(Parent).CutVBorderFromRect(cellRect);
              //нижний бордюр срезаем всегда (ну в зависимости от установленного флага)
              TvpGantt(Parent).CutHBorderFromRect(cellRect);
              TvpGantt(Parent).DrawCell(aRow, Canvas, cellRect);
              //рисуем интервал
              TvpGantt(Parent).Interval[aRow].SetBoundHeight(cellRect.Top, cellRect.Bottom);
              DrawInterval(aRow);
            end;
        end;
    end;
end;

procedure TvpGanttCalendar.DrawRows;
var
  i: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawRows');
  {$endif}
  for i:=0 to TvpGantt(Parent).IntervalCount-1 do
    DrawRow(i);
end;

procedure TvpGanttCalendar.DrawScaleVertLines(const X: integer);
begin
  //рисуем вертикальные разделители времени
  if vpgVertLine in TvpGantt(Parent).Options then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psDot;
      Canvas.Pen.Color := TvpGantt(Parent).BorderColor;
      //рисуем до конца видимой части компонента или до конца списка
      if vpgExtendVertLines in TvpGantt(Parent).Options then
        Canvas.Line(X, TvpGantt(Parent).GetTitleHeight, X, ClientRect.Bottom)
      else
        Canvas.Line(X, TvpGantt(Parent).GetTitleHeight, X, FCalendarHeight);
    end;
end;

procedure TvpGanttCalendar.GetSBVisibility(out HsbVisible, VsbVisible: boolean);
var
  autoVert,autoHorz: boolean;
  ClientW,ClientH: Integer;
  BarW,BarH: Integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetSBVisibility');
  {$endif}
  AutoVert := TvpGantt(Parent).ScrollBarAutomatic(ssVertical);
  AutoHorz := TvpGantt(Parent).ScrollBarAutomatic(ssHorizontal);

  //// get client bounds free of bars
  ClientW  := ClientWidth;
  ClientH  := ClientHeight;
  BarW := TvpGantt(Parent).FScrollBarWidth;
  if ScrollBarIsVisible(SB_VERT) then
    ClientW := ClientW + BarW;
  BarH := TvpGantt(Parent).FScrollBarHeight;
  if ScrollBarIsVisible(SB_HORZ) then
    ClientH := ClientH + BarH;

  // first find out if scrollbars need to be visible by
  // comparing against client bounds free of bars
  HsbVisible := (TvpGantt(Parent).FScrollBars in [ssHorizontal, ssBoth]) or
                (AutoHorz and (FCalendarWidth>ClientW));

  VsbVisible := (TvpGantt(Parent).FScrollBars in [ssVertical, ssBoth]) or
                (AutoVert and (FCalendarHeight>ClientH));

  // then for automatic scrollbars check if grid bounds are
  // in some part of area occupied by scrollbars
  if not HsbVisible and AutoHorz and VsbVisible then
    HsbVisible := FCalendarWidth > (ClientW - BarW);

  if not VsbVisible and AutoVert and HsbVisible then
    VsbVisible := FCalendarHeight > (ClientH - BarH);

  if AutoHorz then
    HsbVisible := HsbVisible; // and not AutoFillColumns;

  {$ifdef DBGGANTTCALENDAR}
  Form1.El.Debug('Horz=%d CalendarW=%d CW=%d BarW=%d', [Integer(HsbVisible), FCalendarWidth, ClientWidth, BarW]);
  Form1.El.Debug('Vert=%d CalendarH=%d CH=%d BarH=%d', [Integer(VsbVisible), FCalendarHeight, ClientHeight, BarH]);
  {$endif}
end;

procedure TvpGanttCalendar.GetSBRanges(const HsbVisible, VsbVisible: boolean;
  out HsbRange, VsbRange, HsbPage, VsbPage, HsbPos, VsbPos: Integer);
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetSBRanges');
  {$endif}
  HsbRange := 0;
  HsbPos := 0;
  if HsbVisible then
    begin
      HsbRange := FCalendarWidth;
      HsbPage := ClientWidth;
    end;

  VsbRange := 0;
  VsbPos := 0;
  if VsbVisible then
    begin
      VsbRange := FCalendarHeight;
      VsbPage := ClientHeight;
    end;

  {$ifdef DBGSCROLL}
  Form1.EL.Debug('GetSBRanges: HRange=%d HPage=%d HPos=%d VRange=%d VPage=%d VPos=%d',
                 [HSbRange, HsbPage, HsbPos, VsbRange, VsbPage, VsbPos]);
  {$endif}
end;

procedure TvpGanttCalendar.InvalidateRow(aRow: integer);
var
  R: TRect;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.InvalidateRow');
  {$endif}
  R := CalcRowRect(aRow);
  InvalidateRect(Handle, @R, true);
end;

procedure TvpGanttCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  curPoint: TPoint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.MouseDown');
  {$endif}
  curPoint := ClientToParent(Point(X, Y), Parent);
  TvpGantt(Parent).MouseDown(Button, Shift, curPoint.X, curPoint.Y);
end;

procedure TvpGanttCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  curPoint: TPoint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.MouseMove');
  {$endif}
  curPoint := ClientToParent(Point(X, Y), Parent);
  TvpGantt(Parent).MouseMove(Shift, curPoint.X, curPoint.Y);
end;

procedure TvpGanttCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  curPoint: TPoint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.MouseUp');
  {$endif}
  curPoint := ClientToParent(Point(X, Y), Parent);
  TvpGantt(Parent).MouseUp(Button, Shift, curPoint.X, curPoint.Y);
end;

procedure TvpGanttCalendar.Paint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.Paint');
  {$endif}
  ClearCanvas;
  DrawRows;
  DrawMajorScale;
  DrawMinorScale;
  DrawCurrentTimeLine;
  DrawEdges;
end;

constructor TvpGanttCalendar.Create(AOwner: TvpGantt);
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.Create');
  {$endif}
  inherited create(AOwner);
  Parent := AOwner;

  FHScrollPosition := 0;

  FMajorScale := vptsMonth;
  FMinorScale := vptsDay;
  FPixelePerMinorScale := C_DEF_PIXEL_PER_MINOR;

  //цвета
  FFreeColor := clLime;
  FExpiredColor := clRed;
end;

destructor TvpGanttCalendar.Destroy;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.Destroy');
  {$endif}
  try
    SetLength(FMinorCountInMajor, 0);
  finally
  end;
  inherited Destroy;
end;

{ TvpGanttTasks }

procedure TvpGanttTasks.UpdateSizes;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.UpdateSizes');
  {$endif}
  FGridTextHeight := Canvas.GetTextHeight('A');
  CalcTasksHeight;
  CalcTasksWidth;
  //сдвигаем правый край, только если сетка нарсиована не с первого диапазона
  //и ширина сетки - скролл горизонтальный < клиентской области
  //текущую позицию сдвига уменьшаем на разницу между шириной клиентской области и видимой области сетки
  if (FHScrollPosition>0) AND (FTasksWidth-FHScrollPosition<ClientWidth) then
    begin
      FHScrollPosition := Max(0, FHScrollPosition - (ClientWidth - FGridVisibleRect.Width));
      {$ifdef DBGSCROLL}
      Form1.Debug('FCalendarWidth-FHScrollPosition<ClientWidth');
      Form1.EL.Debug('FHScrollPosition %d FTasksWidth %d ClientWidth %d - FGridVisibleRect.Right %d ClientRect.Right %d',
                      [FHScrollPosition, FTasksWidth, ClientWidth, FGridVisibleRect.Right, ClientRect.Right]);
      {$endif}
    end;
  //пересчитываем видимую область
  CalcScrollbarsRange;
  CalcGridVisibleRect;
end;

procedure TvpGanttTasks.VisualChange;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.VisualChange');
  {$endif}
  UpdateSizes;
  Invalidate;
end;

function TvpGanttTasks.GetTitleRect: TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.GetTitleRect');
  {$endif}
  result := Rect(0, 0, ClientWidth, TvpGantt(Parent).GetTitleHeight);
end;

procedure TvpGanttTasks.WMSize(var Message: TLMSize);
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.WMSize');
  {$endif}
  if not IsResizing then
    begin
      UpdateSizes;
      inherited;
    end;
end;

procedure TvpGanttTasks.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_HREDRAW;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CreateParams');
  {$endif}
  inherited CreateParams(Params);
  with Params do begin
    WindowClass.Style := WindowClass.Style and DWORD(not ClassStylesOff);
    Style := Style or WS_HSCROLL or WS_CLIPCHILDREN;
  end;
end;

procedure TvpGanttTasks.ClearCanvas;
begin
  {$ifdef DBGGANTTTASKS}
   Form1.Debug('TvpGanttTasks.ClearCanvas');
  {$endif}
  //clear canvas
  Canvas.Brush.Color := FColor;
  Canvas.FillRect(ClientRect);
end;

function TvpGanttTasks.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DoMouseWheel');
  Form1.ShowParam(IntToStr(WheelDelta));
  {$endif}
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TvpGanttTasks.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DoMouseWheelDown');
  {$endif}
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    begin
      TvpGantt(Parent).SelectNextRow(1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

function TvpGanttTasks.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DoMouseWheelUp');
  {$endif}
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    begin
      TvpGantt(Parent).SelectNextRow(-1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

procedure TvpGanttTasks.CalcGridVisibleRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcGridVisibleRect');
  {$endif}
  FGridVisibleRect := ClientRect;
  InflateRect(FGridVisibleRect, -1, 0);
  FGridVisibleRect.Top := FGridVisibleRect.Top + TvpGantt(Parent).GetTitleHeight;
  FGridVisibleRect.Height := Ceil(ClientHeight / TvpGantt(Parent).RowHeight)* TvpGantt(Parent).RowHeight;
end;

function TvpGanttTasks.CalcRowRect(aRow: integer): TRect;
var
  aStart: integer;
  tmpRect: TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcRowRect');
  {$EndIf}
  Result := TRect.Create(0, 0, 0, 0);
  {DONE Рассчитать правую границу в соответствии с ClientWidth
        было FTasksWidth + aRowBorder - FHScrollPosition  стало ClientWidth}
  aStart := TvpGantt(Parent).GetTitleHeight +
            TvpGantt(Parent).RowHeight * aRow - TvpGantt(Parent).FVScrollPosition;
  tmpRect := Rect(FGridVisibleRect.Left - FHScrollPosition, aStart,
                  FGridVisibleRect.Right, aStart + TvpGantt(Parent).RowHeight);
  //обрезаем невидимые части
  IntersectRect(Result, tmpRect, FGridVisibleRect);
end;

procedure TvpGanttTasks.CalcTasksWidth;
var
  i: integer;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcTasksWidth');
  {$EndIf}
  FTasksWidth := 0;
  for i:=0 to TvpGantt(Parent).IntervalCount - 1 do
    FTasksWidth := Max(FTasksWidth, Canvas.TextWidth(TvpGantt(Parent).Interval[i].FName));
  FTasksWidth := Max(FTasksWidth + 2*constCellPadding + TvpGantt(Parent).GetBorderWidth, ClientWidth);
end;

procedure TvpGanttTasks.CalcTasksHeight;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcTasksHeight');
  {$EndIf}
  FTasksHeight := TvpGantt(Parent).GetTitleHeight + TvpGantt(Parent).FIntervalsHeight;
end;

procedure TvpGanttTasks.CalcScrollbarsRange;
var
  HsbVisible: boolean;
  HsbRange, HsbPage, HsbPos: Integer;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcScrollbarsRange');
  {$endif}
  GetSBVisibility(HsbVisible);
  GetSBRanges(HsbVisible, HsbRange, HsbPage, HsbPos);
  UpdateHorzScrollBar(HsbVisible, HsbRange, HsbPage, HsbPos);
end;

procedure TvpGanttTasks.DrawRows;
var
  i: integer;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DrawAllRows');
  {$endif}
  for i:=0 to TvpGantt(Parent).IntervalCount-1 do
    DrawRow(i);
end;

procedure TvpGanttTasks.DrawEdges;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DrawEdges');
  {$endif}
  Canvas.Pen.Width := C_DEF_BORDER_WIDTH;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clActiveBorder;
  Canvas.MoveTo(0, TvpGantt(Parent).GetTitleHeight);
  Canvas.LineTo(0, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right - 1, ClientRect.Bottom - 1);
  Canvas.LineTo(ClientRect.Right - 1, TvpGantt(Parent).GetTitleHeight);
end;

procedure TvpGanttTasks.DrawRow(aRow: integer);
var
  rowRect, cellRect: TRect;
  textPoint: TPoint;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DrawRow');
  {$endif}
  if not Assigned(Parent) then
    Exit;
  //область строки
  rowRect := CalcRowRect(aRow);
  cellRect := TRect.Create(0, 0, 0, 0);
  //шрифт
  Canvas.Font := Font;
  {$ifdef DBGGANTTTASKS}
  Form1.EL.Debug('Row rect Left %d Top %d Right %d Bottom %d',
                  [rowRect.Left, rowRect.Top, rowRect.Right, rowRect.Bottom]);
  Form1.EL.Debug('FVScrollPosition %d FHScrollPosition %d, ClientWidth %d, FGridVisibleRect.Height %d, FTasksWidth %d',
                  [FvpGantt.FVScrollPosition, FHScrollPosition, ClientWidth, FGridVisibleRect.Height, FTasksWidth]);
  {$endif}
  if rowRect.Height>0 then
    begin
      {$ifdef DBGGANTTTASKS}
      Form1.EL.Debug('Drawing row %d', [aRow]);
      {$endif}
      //бордюр
      TvpGantt(Parent).DrawCellGrid(Canvas, rowRect);
      //цвета
      Canvas.Brush.Color := FColor;
      //ячейка cellRect пока не рубим, будем рисовать несколько ячеек и оно нам пригодится
      CopyRect(cellRect, rowRect);
      TvpGantt(Parent).CutHBorderFromRect(cellRect);
      TvpGantt(Parent).DrawCell(aRow, Canvas, cellRect);
      //focus rectangle
      if aRow=TvpGantt(Parent).GetFocusRow then
        TvpGantt(Parent).DrawFocusRect(Canvas, cellRect);
      //считаем сдвиг и выводим текст
      {TODO: -o Vas Сделать расчет вывода текста по вертикали в отдельной функции с учетом вертикальных и горизонтальных линий}
      InflateRect(cellRect, -constCellPadding, 0);
      textPoint.X := cellRect.Left - FHScrollPosition;
      textPoint.Y := cellRect.Bottom - FGridTextHeight - (TvpGantt(Parent).RowHeight - TvpGantt(Parent).GetHorzBorderWidth - FGridTextHeight) div 2;
      Canvas.TextRect(cellRect, textPoint.X, textPoint.Y, TvpGantt(Parent).Interval[aRow].Name);
    end;
end;

procedure TvpGanttTasks.DrawTitle;
var
  titRect: TRect;
  titCaption: PChar;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DrawTitle');
  {$endif}
  titRect := GetTitleRect;
  titCaption := PChar(TvpGantt(Parent).TaskTitleCaption);
  if not TvpGantt(Parent).FTitleFontIsDefault then
    Canvas.Font.Assign(TvpGantt(Parent).TitleFont)
  else
    Canvas.Font := Font;
  TvpGantt(Parent).DrawTitleCell(Canvas, titRect);
  TvpGantt(Parent).DrawTitleText(Canvas, titRect, titCaption);
end;

procedure TvpGanttTasks.GetSBVisibility(out HsbVisible: boolean);
var
  autoHorz: boolean;
  ClientW, ClientH: Integer;
  BarH: Integer;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.GetSBVisibility');
  {$endif}
  AutoHorz := TvpGantt(Parent).ScrollBarAutomatic(ssHorizontal);

  // get client bounds free of bars
  ClientW  := ClientWidth;
  ClientH  := ClientHeight;
  BarH := TvpGantt(Parent).FScrollBarHeight;
  if ScrollBarIsVisible(SB_HORZ) then
    ClientH := ClientH + BarH;

  // first find out if scrollbars need to be visible by
  // comparing against client bounds free of bars
  HsbVisible := (TvpGantt(Parent).ScrollBars in [ssBoth, ssHorizontal]) or
                (AutoHorz and (FTasksWidth>ClientW));

  // then for automatic scrollbars check if grid bounds are
  // in some part of area occupied by scrollbars
  if not HsbVisible and AutoHorz then
    HsbVisible := FTasksWidth  > ClientW;

  if AutoHorz then
    HsbVisible := HsbVisible; // and not AutoFillColumns;

  {$ifdef DBGGANTTTASKS}
  Form1.El.Debug('Horz=%d TasksH=%d CH=%d BarH=%d', [Integer(HsbVisible), FTasksHeight, ClientHeight, BarH]);
  {$endif}
end;

procedure TvpGanttTasks.GetSBRanges(const HsbVisible: boolean; out
  HsbRange, HsbPage, HsbPos: Integer);
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.GetSBRanges');
  {$endif}
  //calculate only horizontal
  HsbRange := 0;
  HsbPos := 0;
  if HsbVisible then
    begin
      HsbRange := FTasksWidth;
      HsbPage := ClientWidth;
    end;

  {$ifdef DBGGANTTTASKS}
  Form1.Debug(Format('HsbRange %d', [HsbRange]));
  Form1.Debug(Format('FTasksWidth %d', [FTasksWidth]));
  {$endif}
end;

procedure TvpGanttTasks.InvalidateRow(aRow: integer);
var
  R: TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.InvalidateRow');
  {$endif}
  R := CalcRowRect(aRow);
  InvalidateRect(Handle, @R, true);
end;

procedure TvpGanttTasks.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  curPoint: TPoint;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.MouseDown');
  {$endif}
  curPoint := ClientToParent(Point(X, Y), Parent);
  TvpGantt(Parent).MouseDown(Button, Shift, curPoint.X, curPoint.Y);
end;

procedure TvpGanttTasks.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  curPoint: TPoint;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.MouseMove');
  {$endif}
  curPoint := ClientToParent(Point(X, Y), Parent);
  TvpGantt(Parent).MouseMove(Shift, curPoint.X, curPoint.Y);
end;

procedure TvpGanttTasks.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  curPoint: TPoint;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.MouseUp');
  {$endif}
  curPoint := ClientToParent(Point(X, Y), Parent);
  TvpGantt(Parent).MouseUp(Button, Shift, curPoint.X, curPoint.Y);
end;

procedure TvpGanttTasks.Paint;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.Paint');
  {$endif}
  ClearCanvas;
  DrawRows;
  DrawTitle;
  DrawEdges;
end;

function TvpGanttTasks.ScrollBarIsVisible(Which: Integer): Boolean;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.ScrollBarIsVisible');
  {$endif}
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
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.ScrollBarPosition');
  {$endif}
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
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.ScrollBarRange');
  {$endif}
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
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.ScrollBarShow');
  {$endif}
  if HandleAllocated then begin
    ShowScrollBar(Handle,Which,aValue);
    if Which in [SB_BOTH, SB_HORZ] then FHSbVisible := AValue;
  end;
end;

procedure TvpGanttTasks.UpdateHorzScrollBar(const aVisible: boolean;
  const aRange, aPage, aPos: Integer);
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.UpdateHorzScrollBar');
  {$endif}
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
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.WMHScroll');
  {$endif}
  if not HandleAllocated then
    exit;

  if csDesigning in ComponentState then
    exit;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_POS;
  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  maxPos := ScrollInfo.nMax - Max(ScrollInfo.nPage-1, 0);

  aCode := message.ScrollCode;
  aPos := ScrollInfo.nPos;

  {$ifdef DBGGANTTTASKS}
  Form1.Debug(Format('maxPos %d', [maxPos]));
  Form1.Debug(Format('aPos %d', [aPos]));
  {$endif}
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

  {$ifdef DBGGANTTTASKS}
  Form1.Debug(Format('aPos %d', [aPos]));
  Form1.Debug(Format('FHScrollPosition %d', [FHScrollPosition]));
  {$endif}

  ScrollBarPosition(SB_HORZ, aPos);
  FHScrollPosition := aPos;

  {$ifdef DBGGANTTTASKS}
  Form1.Debug(Format('FHScrollPosition %d', [FHScrollPosition]));
  Form1.Debug(Format('FHScrollPosition %d', [FHScrollPosition]));
  {$endif}

  Invalidate;
end;

constructor TvpGanttTasks.Create(AOwner: TvpGantt);
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.Create');
  {$endif}
  inherited create(AOwner);
  Parent := AOwner;
  //default value
  FHScrollPosition := 0;
end;

destructor TvpGanttTasks.Destroy;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.Destroy');
  {$endif}
  inherited Destroy;
end;

procedure TvpGanttTasks.InvalidateTitle;
var
  R: TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.InvalidateTitle');
  {$endif}
  if not HandleAllocated then
    exit;
  R := GetTitleRect;
  InvalidateRect(Handle, @R, False);
end;

{ TvpGantt }

procedure TvpGantt.ClearBoundDates;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.ClearDates');
  {$EndIf}
  FStartDateOfBound := 0;
  FEndDateOfBound := 0;
end;

function TvpGantt.GetCalendarColor: TColor;
begin
  Result := FvpGanttCalendar.FColor;
end;

procedure TvpGantt.CalcIntervalsHeight;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.CalcIntervalsHeight');
  {$EndIf}
  FIntervalsHeight := IntervalCount * RowHeight;
end;

procedure TvpGantt.CalcFocusRect;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.CalcFocusRect');
  {$endif}
  {$ifdef DBGGANTTTASKS}
  Form1.EL.Debug('FFocusRow %d RowHeight %d FGanttBorderWidth %d GetTitleHeight %d FVScrollPosition %d',
                  [FFocusRow, RowHeight, FGanttBorderWidth, GetTitleHeight, FVScrollPosition]);
  {$EndIf}
  FFocusRect := ClientRect;
  FFocusRect.Top := FFocusRow * RowHeight + GetTitleHeight - FVScrollPosition;
  if vpgHorzLine in Options then
    FFocusRect.Height := RowHeight - GetBorderWidth;
  if vpgVertLine in Options then
    FFocusRect.Right := FFocusRect.Right - GetBorderWidth;
  {$ifdef DBGGANTTTASKS}
  Form1.EL.Debug('FFocusRect LTRB %d %d %d %d', [FFocusRect.Left, FFocusRect.Top, FFocusRect.Right, FFocusRect.Bottom]);
  {$EndIf}
end;

procedure TvpGantt.CutBorderFromRect(var aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.CalcCellRectWoBorder');
  {$EndIf}
  CutHBorderFromRect(aRect);
  CutVBorderFromRect(aRect);
end;

procedure TvpGantt.CutHBorderFromRect(var aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.CutHBorderFromRect');
  {$EndIf}
  aRect.Bottom := aRect.Bottom - GetHorzBorderWidth;
end;

procedure TvpGantt.CutVBorderFromRect(var aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.CutVBorderFromRect');
  {$EndIf}
  aRect.Right := aRect.Right - GetVertBorderWidth;
end;

procedure TvpGantt.DoClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TvpGantt.DoDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;


function TvpGantt.GetIntervalsHeight: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetIntervalsHeight');
  {$EndIf}
  Result := FIntervalsHeight;
end;

function TvpGantt.GetIntervalCount: Integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetIntervalCount');
  {$endif}
  Result := 0;
  if Assigned(FIntervals) then
    Result := FIntervals.Count;
end;

function TvpGantt.GetInterval(AnIndex: Integer): TvpInterval;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetInterval');
  {$endif}
  Result := TvpInterval(FIntervals[AnIndex]);
end;

procedure TvpGantt.OnTitleFontChanged(Sender: TObject);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.OnTitleFontChanged');
  {$endif}
  FTitleFontIsDefault := False;
  VisualChange;
end;

function TvpGantt.OptionsIsStored: Boolean;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.OptionsIsStored');
  {$endif}
  Result := FvpGanttOptions <> DefaultGanttOptions;
end;

procedure TvpGantt.SetDateFormat(AValue: String);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetDateFormat');
  {$endif}
  if Trim(AValue) = '' then
    AValue := DefaultFormatSettings.ShortDateFormat + ' ' +
              DefaultFormatSettings.ShortTimeFormat;
  if FDateFormat = AValue then
    Exit;
  FDateFormat := AValue;
end;

procedure TvpGantt.SetEndDate(AValue: TDate);
var
  aDT: TDateTime;
  aYear, aMonth, ADay: word;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetEndDate');
  {$endif}
  //декодируем в числа и получаем конец дня
  DecodeDate(AValue, aYear, aMonth, ADay);
  aDT := EndOfADay(aYear, aMonth, ADay);
  if FEndDate = aDT then
    Exit;
  if csReading in ComponentState then
    FEndDate := aDT
  //если текущее время меньше начального, то устанавливаем ему начало этогоже дня
  else
    begin
      FEndDate := aDT;
      if FEndDate<FStartDate then
        FStartDate := StartOfTheDay(aDT);
      UpdateBoundDates(FStartDate, FEndDate);
      VisualChange;
    end;
end;

procedure TvpGantt.SetExpiredIntervalColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetExpiredIntervalColor');
  {$endif}
  if FExpiredIntervalColor = AValue then
    Exit;
  FExpiredIntervalColor := AValue;
  VisualChange;
end;

procedure TvpGantt.SetFactIntervalColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetFactIntervalColor');
  {$endif}
  if FFactIntervalColor = AValue then
    Exit;
  FFactIntervalColor := AValue;
  VisualChange;
end;

procedure TvpGantt.SetFocusColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetFocusColor');
  {$endif}
  if FFocusColor = AValue then
    Exit;
  FFocusColor := AValue;
  //DONE Сделать обновление области с фокусом
  InvalidateFocused;
end;

procedure TvpGantt.SetFreeIntervalColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetFreeIntervalColor');
  {$endif}
  if FFreeIntervalColor = AValue then
    Exit;
  FFreeIntervalColor := AValue;
  VisualChange;
end;

procedure TvpGantt.SetMajorScale(AValue: TvpTimeScale);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetMajorScale');
  {$endif}
  if not FvpGanttCalendar.HandleAllocated then
    Exit;
  if csReading in ComponentState then
    FvpGanttCalendar.FMajorScale := AValue
  else if AValue < FvpGanttCalendar.FMinorScale then
    raise Exception.Create(RS_E_MAJORSCALE_LOW)
  else if AValue = FvpGanttCalendar.FMinorScale then
    raise Exception.Create(RS_E_MAJORSCALE_EQUAL)
  else
    begin
      FvpGanttCalendar.FMajorScale := AValue;
      //обнуляем шкалу для правильного пересчета
      ClearBoundDates;
      UpdateBoundDates(FStartDate, FEndDate);
      VisualChange;
    end;
end;

procedure TvpGantt.SetMinorScale(AValue: TvpTimeScale);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetMinorScale');
  {$endif}
  if not FvpGanttCalendar.HandleAllocated then
    Exit;
  if csReading in ComponentState then
    FvpGanttCalendar.FMinorScale := AValue
  else if AValue > FvpGanttCalendar.FMajorScale then
    raise Exception.Create(RS_E_MINORSCALE_HIGH)
  else if AValue = FvpGanttCalendar.FMajorScale then
    raise Exception.Create(RS_E_MINORSCALE_EQUAL)
  else
    begin
      FvpGanttCalendar.FMinorScale := AValue;
      //обнуляем шкалу для правильного пересчета
      ClearBoundDates;
      UpdateBoundDates(FStartDate, FEndDate);
      VisualChange;
    end;
end;

procedure TvpGantt.SetMajorScaleHeight(AValue: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetMajorScaleHeight');
  {$endif}
  if FMajorScaleHeight = AValue then
    Exit;
  FMajorScaleHeight := AValue;
  VisualChange;
end;

procedure TvpGantt.SetMinorScaleHeight(AValue: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetMinorScaleHeight');
  {$endif}
  if FMinorScaleHeight = AValue then
    Exit;
  FMinorScaleHeight := AValue;
  VisualChange;
end;

procedure TvpGantt.SetOptions(AValue: TvpGanttOptions);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetOptions');
  {$endif}
  if FvpGanttOptions = AValue then Exit;
  FvpGanttOptions := AValue;
  VisualChange;
end;

procedure TvpGantt.SetPixelPerMinorScale(AValue: integer);
begin
  if (not FvpGanttCalendar.HandleAllocated) OR (FvpGanttCalendar.FPixelePerMinorScale = AValue) then
    Exit;
  FvpGanttCalendar.FPixelePerMinorScale := AValue;
  FvpGanttCalendar.Invalidate;
end;

procedure TvpGantt.SetPlanIntervalColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetPlanIntervalColor');
  {$endif}
  if FPlanIntervalColor = AValue then
    Exit;
  FPlanIntervalColor := AValue;
  VisualChange;
end;

procedure TvpGantt.SetRowHeight(AValue: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetRowHeight');
  {$endif}
  if FRowHeight = AValue then Exit;
  FRowHeight := AValue;
  VisualChange;
end;

procedure TvpGantt.SetGridBorderWidth(AValue: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetBorderWidth');
  {$endif}
  if FGridBorderWidth = AValue then
    Exit;
  FGridBorderWidth := AValue;
  VisualChange;
end;

procedure TvpGantt.SetBorderColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetBorderColor');
  {$endif}
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;
  VisualChange;
end;

procedure TvpGantt.SetStartDate(AValue: TDate);
var
  aDT: TDateTime;
  aYear, aMonth, ADay: word;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetStartDate');
  {$endif}
  //декодируем в числа и получаем начало дня
  DecodeDate(AValue, aYear, aMonth, ADay);
  aDT := StartOfADay(aYear, aMonth, ADay);
  if FStartDate = aDT then
    Exit;
  if csReading in ComponentState then
    FStartDate := aDT
  //если текущее время больше конечного, то устанавливаем ему конец этого же дня
  else
    begin
      FStartDate := aDT;
      if FStartDate>FEndDate then
        FEndDate := EndOfTheDay(aDT);
      UpdateBoundDates(FStartDate, FEndDate);
      VisualChange;
    end;
end;

procedure TvpGantt.SetTaskColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetTaskColor');
  {$endif}
  if FvpGanttTasks.FColor=AValue then
    Exit;
  FvpGanttTasks.FColor := AValue;
  if FUpdateCount=0 then
    FvpGanttTasks.Invalidate;
end;

procedure TvpGantt.SetCalendarColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetCalendarColor');
  {$endif}
  if FvpGanttCalendar.FColor=AValue then
    Exit;
  FvpGanttCalendar.FColor := AValue;
  if (FUpdateCount=0) AND FvpGanttCalendar.HandleAllocated then
    FvpGanttCalendar.Invalidate;
end;

procedure TvpGantt.SetScrollBars(const AValue: TScrollStyle);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetScrollBars');
  {$endif}
  if FScrollBars=AValue then
    Exit;
  FScrollBars := AValue;
  VisualChange;
end;

procedure TvpGantt.SetTaskTitleCaption(AValue: TCaption);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetTaskTitleCaption');
  {$endif}
  if FTaskTitleCaption=AValue then Exit;
  FTaskTitleCaption := AValue;
  if FvpGanttTasks.HandleAllocated then
    FvpGanttTasks.VisualChange;
end;

procedure TvpGantt.SetTitleColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetTitleColor');
  {$endif}
  if FTitleColor = AValue then Exit;
  FTitleColor := AValue;
  VisualChange;
end;

procedure TvpGantt.SetTitleFont(const AValue: TFont);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetTitleFont');
  {$endif}
  FTitleFont.Assign(AValue);
  VisualChange;
end;

procedure TvpGantt.SetTitleStyle(const AValue: TTitleStyle);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetTitleStyle');
  {$endif}
  if FTitleStyle=AValue then exit;
  FTitleStyle:=AValue;
  Invalidate;
end;

procedure TvpGantt.SetFocus;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetFocus');
  {$endif}
  if not Focused and not(csNoFocus in ControlStyle) then
    begin
      inherited;
      InvalidateFocused;
    end;
  //else
  //  begin
  //    {$ifDef DBGGANTTCALENDAR}
  //    DebugLnExit('TvpGanttCalendar.MouseDown EXIT: Focus not allowed');
  //    {$Endif}
  //    Exit;
  //  end;
end;

procedure TvpGantt.InvalidateFocused;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.InvalidateFocused');
  {$endif}
  if FUpdateCount=0 then
    InvalidateRow(FFocusRow);
end;

procedure TvpGantt.InvalidateRow(aRow: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.InvalidateRow');
  {$endif}
  if FvpGanttTasks.HandleAllocated then
    FvpGanttTasks.InvalidateRow(aRow);
  if FvpGanttCalendar.HandleAllocated then
    FvpGanttCalendar.InvalidateRow(aRow);
end;

procedure TvpGantt.KeyDown(var Key: Word; Shift: TShiftState);
var
  Sh: Boolean;
  DeltaRow: Integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.KeyDown INIT');
  {$endif}
  inherited KeyDown(Key, Shift);

  Sh := (ssShift in Shift);

  case Key of
    //VK_TAB:
    //  if goTabs in Options then begin
    //    if GetDeltaMoveNext(Sh, DeltaCol,DeltaRow,FTabAdvance) then begin
    //      Sh := False;
    //      MoveSel(True, DeltaCol, DeltaRow);
    //      PreserveRowAutoInserted := True;
    //      Key:=0;
    //    end else if (goAutoAddRows in Options) and (DeltaRow = 1) then begin
    //      //prevent selecting multiple cells when user presses Shift
    //      Sh := False;
    //      if (goAutoAddRowsSkipContentCheck in Options) or (not IsEmptyRow(Row)) then MoveSel(True, DeltaCol, DeltaRow);
    //      Key := 0;
    //      PreserveRowAutoInserted := True;
    //    end else
    //    if (TabAdvance=aaNone) or
    //       ((TabAdvance=aaDown) and (Row>=GetLastVisibleRow)) or
    //       (sh and (Col<=GetFirstVisibleColumn)) or
    //       ((not sh) and (Col>=GetLastVisibleColumn)) then
    //      TabCheckEditorKey
    //    else
    //      Key := 0;
    //  end else
    //    TabCheckEditorKey;
    //VK_LEFT:
    //  //Don't move to another cell is user is editing
    //  if not FEditorKey then
    //  begin
    //    if Relaxed then
    //      MoveSel(True, -cBidiMove[UseRightToLeftAlignment], 0)
    //    else
    //      MoveSel(True, 0,-1);
    //  end;
    //VK_RIGHT:
    //  //Don't move to another cell is user is editing
    //  if not FEditorKey then
    //  begin
    //    if Relaxed then
    //      MoveSel(True, cBidiMove[UseRightToLeftAlignment], 0)
    //    else
    //      MoveSel(True, 0, 1);
    //  end;
    VK_UP:
        SelectNextRow(-1);
    VK_DOWN:
        SelectNextRow(1);
    VK_PRIOR:
      begin
        //кол-во видимых строк
        DeltaRow := Trunc((ClientRect.Height - GetTitleHeight - FScrollBarHeight) / RowHeight);
        SelectNextRow(-DeltaRow);
        {TODO -oVas: неправильно работает прокрутка постраничная}
      end;
    VK_NEXT:
      begin
        DeltaRow := Trunc((ClientRect.Height - GetTitleHeight - FScrollBarHeight) / RowHeight);
        SelectNextRow(DeltaRow);
      end;
    VK_HOME:
      SelectNextRow(-FFocusRow);
    VK_END:
      SelectNextRow(IntervalCount-FFocusRow-1);
    //VK_C:
    //  if not FEditorKey and (Shift = [ssModifier]) then
    //    doCopyToClipboard;
    //VK_V:
    //  if not FEditorKey and (Shift = [ssModifier]) then
    //    doPasteFromClipboard;
    //VK_X:
    //  if not FEditorKey and (Shift = [ssShift]) then
    //    doCutToClipboard;
    //VK_DELETE:
    //  if not FEditorKey and EditingAllowed(FCol)
    //  and (Editor is TCustomEdit) and not (csDesigning in ComponentState)
    //  then begin
    //    EditorShow(False);
    //    TCustomEdit(Editor).Text:='';
    //    InvalidateCell(FCol,FRow,True);
    //    EditorShow(True);
    //    Key := 0;
    //  end;
  end;
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.KeyDown END');
  {$endif}
end;

procedure TvpGantt.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TvpGantt.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
end;

procedure TvpGantt.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  aRow: integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.MouseDown');
  Form1.EL.Debug('x %d y %d',[X, Y]);
  {$endif}
  aRow := GetRowPosY(Y);
  if aRow>-1 then
    SetFocusRow(aRow);
  SetFocus;
end;

procedure TvpGantt.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aRow: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.MouseMove');
  Form1.EL.Debug('x %d y %d',[X, Y]);
  {$endif}
  inherited MouseMove(Shift, X, Y);

  if csDesigning in ComponentState then
    Exit;

  aRow := GetRowPosY(Y);
  FMouseInterval := aRow;

  if ShowHint AND (FMouseInterval>-1) then
    begin
      Application.CancelHint;
      ShowRowHintWindow(Point(X,Y));
    end
  else
    Hint := FSavedHint;
end;

procedure TvpGantt.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.MouseUp');
  Form1.EL.Debug('x %d y %d',[X, Y]);
  {$endif}
  inherited MouseUp(Button, Shift, X, Y);
end;

function TvpGantt.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.ScrollBarAutomatic');
  {$endif}
  Result := false;
  if (Which=ssVertical) OR (Which=ssHorizontal) then
    begin
      if Which=ssVertical then
        Which:=ssAutoVertical
      else
        Which:=ssAutoHorizontal;
      Result:= FScrollBars in [Which, ssAutoBoth];
    end;
end;

procedure TvpGantt.VisualChange;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.VisualChange INIT');
  {$endif}
  UpdateSizes;
  if (FUpdateCount=0) AND HandleAllocated then
    begin
      {$ifdef DBGGANTT}
      Form1.Debug('TvpGantt.VisualChange WORK');
      {$endif}
      if FvpGanttTasks.HandleAllocated then
        FvpGanttTasks.VisualChange;
      if FvpGanttCalendar.HandleAllocated then
        FvpGanttCalendar.VisualChange;
    end;
end;

procedure TvpGantt.CMMouseEnter(var Message: TLMessage);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.CMMouseEnter');
  {$endif}
  inherited;
  FSavedHint := Hint;
end;

procedure TvpGantt.CMMouseLeave(var Message: TLMessage);
begin
  Hint := FSavedHint;
  inherited CMMouseLeave(Message);
end;

procedure TvpGantt.UpdateSizes;
var
  heightWithHScrollBar, visHeight: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.UpdateSizes');
  {$endif}
  CalcIntervalsHeight;
  //считаем сдвиг по вертикали
  if FvpGanttTasks.HandleAllocated AND FvpGanttCalendar.HandleAllocated then
    begin
      visHeight := FIntervalsHeight + GetTitleHeight - FVScrollPosition;
      if (FvpGanttTasks.FHSbVisible) OR (FvpGanttCalendar.FHSbVisible) then
        heightWithHScrollBar := ClientHeight - FScrollBarHeight
      else
        heightWithHScrollBar := ClientHeight;
      if (visHeight<heightWithHScrollBar) then
        begin
          FVScrollPosition := FVScrollPosition - (heightWithHScrollBar - visHeight);
          FVScrollPosition := max(0, FVScrollPosition);
          SendMessage(FvpGanttCalendar.Handle, LM_VSCROLL, MakeWParam(SB_THUMBPOSITION, FVScrollPosition),0);
        end;
    end;
end;

procedure TvpGantt.WMSize(var message: TLMSize);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.WMSize');
  {$endif}
  if not IsResizing then
    begin
      UpdateSizes;
      inherited;
    end;
end;

procedure TvpGantt.WMKillFocus(var message: TLMKillFocus);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.WMKillFocus');
  {$endif}
  InvalidateFocused;
end;

procedure TvpGantt.WMSetFocus(var message: TLMSetFocus);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.WMSetFocus');
  {$endif}
  {$ifdef DBGFOCUS}
  Form1.Debug('*** gantt.WMSetFocus, FocusedWnd=' + dbgs(Message.FocusedWnd) + ' [' + dbgs(pointer(Message.FocusedWnd))+'] ');
  if Message.FocusedWnd=Self.Handle then
    Form1.EL.Debug('Same Grid!')
  else
    Form1.EL.Debug('ExternalWindow');
  {$endif}
  inherited WMSetFocus(Message);
  InvalidateFocused;
end;

function TvpGantt.GetBorderWidth: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetBorderWidth');
  {$endif}
  if FGridBorderWidth > 0 then
    Result := FGridBorderWidth
  else
    Result := 0;
end;

function TvpGantt.GetHorzBorderWidth: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetHorzBorderWidth');
  {$endif}
  if vpgHorzLine in Options then
    Result := GetBorderWidth
  else
    Result := 0;
end;

function TvpGantt.GetVertBorderWidth: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetVertBorderWidth');
  {$endif}
  if vpgVertLine in Options then
    Result := GetBorderWidth
  else
    Result := 0;
end;

function TvpGantt.GetSelectedIndex: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetIntervalIndex');
  {$endif}
  Result := FFocusRow;
end;

function TvpGantt.GetMajorScale: TvpTimeScale;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetMajorScale');
  {$endif}
  if not FvpGanttCalendar.HandleAllocated then
    Result := vptsDay
  else
    Result := FvpGanttCalendar.FMajorScale;
end;

function TvpGantt.GetMajorScaleHeight: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetMajorScaleHeight');
  {$endif}
  Result := FMajorScaleHeight;
end;

function TvpGantt.GetMinorScale: TvpTimeScale;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetMinorScale');
  {$endif}
  if not FvpGanttCalendar.HandleAllocated then
    Result := vptsHour
  else
    Result := FvpGanttCalendar.FMinorScale;
end;

function TvpGantt.GetPixelPerMinorScale: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetPixelPerMinorScale');
  {$endif}
  if FvpGanttCalendar.HandleAllocated then
    Result := FvpGanttCalendar.FPixelePerMinorScale
  else
    Result := C_DEF_PIXEL_PER_MINOR;
end;

function TvpGantt.GetTaskColor: TColor;
begin
  Result := FvpGanttTasks.FColor;
end;

function TvpGantt.GetStartDateOfBound: TDateTime;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetStartIntervalDate');
  {$endif}
  Result := FStartDateOfBound;
end;

function TvpGantt.GetEndDateOfBound: TDateTime;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetEndIntervalDate');
  {$endif}
  Result := FEndDateOfBound;
end;

{ Процедура пересчета временных переменных
}
procedure TvpGantt.UpdateDates;
var
  i: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetMajorScale');
  {$endif}
  //обнуляем время
  FStartDateOfBound := 0;
  FEndDateOfBound := 0;
  //расчитываем по новой для каждого интервала
  for i:=0 to IntervalCount-1 do
    UpdateInterval(i);
  VisualChange;
end;

function TvpGantt.Focused: boolean;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.Focused');
  {$endif}
  Result := inherited Focused;
  {$ifdef DBGFOCUS}
  Form1.EL.Debug('Result inherited %d', [Integer(Result)]);
  {$endif}
  Result := Result AND CanTab;
  {$ifdef DBGFOCUS}
  Form1.EL.Debug('Result FindOwnerControl %d', [Integer(Result)]);
  {$endif}
  //если задачник в фокусе
  if FvpGanttTasks.HandleAllocated then
    Result := Result OR FvpGanttTasks.Focused;
  {$ifdef DBGFOCUS}
  Form1.EL.Debug('Result FvpGanttTasks %d', [Integer(Result)]);
  {$endif}
  //или календарь
  if FvpGanttCalendar.HandleAllocated then
    Result := Result OR FvpGanttCalendar.Focused;
  {$ifdef DBGFOCUS}
  Form1.EL.Debug('Result FvpGanttCalendar %d', [Integer(Result)]);
  {$endif}
end;

function TvpGantt.GetMinorScaleHeight: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetMajorScale');
  {$endif}
  Result := FMinorScaleHeight;
end;

function TvpGantt.GetTaskTitleCaption: TCaption;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetTaskTitleCaption');
  {$endif}
  Result := FTaskTitleCaption;
end;

procedure TvpGantt.Paint;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.Paint');
  {$endif}
  //отрисовка :)
end;

procedure TvpGantt.FontChanged(Sender: TObject);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.FontChanged');
  {$endif}
  if csCustomPaint in ControlState then
    Canvas.Font := Font
  else
    begin
      inherited FontChanged(Sender);
      if FTitleFontIsDefault then begin
        FTitleFont.Assign(Font);
        FTitleFontIsDefault := True;
      end;
  end;
end;

function TvpGantt.GetFocusRow: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetFocusRow');
  {$endif}
  Result := FFocusRow;
end;

procedure TvpGantt.SetFocusRow(AValue: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetFocusRow');
  {$endif}
  if (FFocusRow=AValue) then
    Exit;
  //если список пустой  или AValue <0
  if (IntervalCount=0) OR (AValue<0) then
    begin
      FFocusRow := -1;
      Exit
    end;
  if AValue>IntervalCount-1 then
    raise Exception.Create(RS_E_LIST_INDEX_OUT_OF_BOUNDS)
  else
    begin
      //стираем текущий фокус
      InvalidateFocused;
      FFocusRow := AValue;
      CalcFocusRect;
      if (FUpdateCount=0) AND (FvpGanttCalendar.HandleAllocated) then
        FvpGanttCalendar.ScrollToFocus;
    end;
end;

procedure TvpGantt.SelectNextRow(const ADelta: integer);
var
  aRow: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SelectNextRow');
  {$endif}
  //если стоим на первой строкt или -1 и прокрутка вверх, то ничего не делаем
  if (FFocusRow<=0) AND (ADelta<0) then
    Exit;
  //если стоим в конце диапазона и прокрутка вних, то тоже ничего не делаем
  if (FFocusRow=IntervalCount-1) AND (ADelta>0) then
    Exit;
  aRow := FFocusRow + ADelta;
  aRow := Max(0, aRow);
  aRow := Min(aRow, IntervalCount-1);
  SetFocusRow(aRow);
end;

procedure TvpGantt.ShowRowHintWindow(APoint: TPoint);
var
  txt, AppHint: string;
  duration, complete: Double;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.ShowRowHintWindow');
  {$endif}
  //если не показывать хинт для строк или строки отсутсвубт под курсором мыши
  if not (vpgRowHint in Options) OR (FMouseInterval<0) then
    Exit;
  //строим подсказку
  txt := TvpInterval(FIntervals[FMouseInterval]).Name;
  txt := txt + LineEnding + RS_HINT_STARTDATE + FormatDateTime(DateFormat, TvpInterval(FIntervals[FMouseInterval]).StartDate);
  //продолжительность в часах
  duration := RoundTo(TvpInterval(FIntervals[FMouseInterval]).Duration * 24, -2);
  txt := txt + LineEnding + RS_HINT_DURATION + FloatToStr(duration) + ' ' + RS_HOUR;
  if TvpInterval(FIntervals[FMouseInterval]).FFinishDate>0 then
    begin
      txt := txt + LineEnding + RS_HINT_FINISHDATE + FormatDateTime(DateFormat, TvpInterval(FIntervals[FMouseInterval]).FinishDate);
      complete := RoundTo(TvpInterval(FIntervals[FMouseInterval]).Complete * 24, -2);
      txt := txt + LineEnding + RS_HINT_COMPLETE + FloatToStr(complete) + ' ' + RS_HOUR;
    end;
  if (txt = '') and (FSavedHint <> '') then
    txt := FSavedHint;
  if (AppHint = '') then
    AppHint := FSavedHint;
  if (txt <> '') and not (csDesigning in ComponentState) then
    begin
      Hint := txt;
      Application.Hint := AppHint;
      Application.ActivateHint(ClientToScreen(APoint));
    end
  else
    HideRowHintWindow;
end;

{ Расчет начального времени интервалов и конечного. Они могут не совпадать с заданными датами.
  Например, дата начальная 12 января 2019 года, дака коенчная 23 марта 2019 года, старшаа шкала
  задана как vptsMonth, тогда нам надо сделать количество диапазонов в старшей шкале кратное месяцам, это
  будет январь, февраль, март. Значит начало интервала отрисовки будет с 01.01.2019 по 31.03.2019
  Будем использовать процедуру всегда:
    1) при смене даты
    2) при смене любой из шкал
    3) при измненении интервалов
}
procedure TvpGantt.UpdateBoundDates(AStartDate, AEndDate: TDate);
var
  aSYear, aSMonth, aSDay: word;
  aEYear, aEMonth, aEDay: word;
begin
  {$ifdef DBGGANTT}
  Form1.EL.Debug('TvpGantt.UpdateIntervalDates %s %s', [DateToStr(AStartDate), DateToStr(AEndDate)]);
  {$endif}
  //выбираем меньшую дату между занятой и устанавливаемой
  AStartDate := Min(AStartDate, StartDate);
  //если уже период считали, то проверяем не получили ли мы диапазон уже, чем в прошлый раз
  if FStartDateOfBound<>0 then
    AStartDate := Min(AStartDate, FStartDateOfBound);
  DecodeDate(AStartDate, aSYear, aSMonth, aSDay);
  //а вдруг как-то передадим дату начала > даты окончания, не порядок
  AEndDate := Max(AEndDate, EndDate);
  //если уже период считали, то проверяем не получили ли мы диапазон уже, чем в прошлый раз
  if FEndDateOfBound<>0 then
    AEndDate := Max(AEndDate, FEndDateOfBound);
  //если полученный конец периода меньше начала, то непорядок
  if AEndDate<AStartDate then
    AEndDate := AStartDate;
  DecodeDate(AEndDate, aEYear, aEMonth, aEDay);
  case MajorScale of
    //берем начало дня
    //концом диапазона будем считать день в 23:59:59
    //минуты быть не могут, но возьмем и их
    vptsMinute, vptsDecMinute, vptsHour, vptsDay, vptsDayWeek:
      begin
       FStartDateOfBound := StartOfADay(aSYear, aSMonth, aSDay);
       FEndDateOfBound := EndOfADay(aEYear, aEMonth, aEDay);
      end;
    //недели
    vptsWeek, vptsWeekNum, vptsWeekNumPlain:
      begin
        FStartDateOfBound := StartOfAWeek(aSYear, aSMonth, aSDay);
        FEndDateOfBound := EndOfAWeek(aEYear, aEMonth, aEDay);
      end;
    //месяц
    vptsMonth:
      begin
        FStartDateOfBound := StartOfAMonth(aSYear, aSMonth);
        FEndDateOfBound := EndOfAMonth(aEYear, aEMonth);
      end;
    //квартал
    vptsQuarter:
      begin
        case aSMonth of
          1..3: FStartDateOfBound := StartOfAMonth(aSYear, 1);
          4..6: FStartDateOfBound := StartOfAMonth(aSYear, 4);
          7..9: FStartDateOfBound := StartOfAMonth(aSYear, 7);
          10..12: FStartDateOfBound := StartOfAMonth(aSYear, 10);
        end;
        case aSMonth of
          1..3: FEndDateOfBound := EndOfAMonth(aEYear, 3);
          4..6: FEndDateOfBound := EndOfAMonth(aEYear, 6);
          7..9: FEndDateOfBound := EndOfAMonth(aEYear, 9);
          10..12: FEndDateOfBound := EndOfAMonth(aEYear, 12);
        end;
      end;
    //полугодия
    vptsHalfYear:
      if aSMonth<7 then
        begin
          FStartDateOfBound := StartOfAMonth(aSYear, 1);
          FEndDateOfBound := EndOfAMonth(aEYear, 6);
        end
      else
        begin
          FStartDateOfBound := StartOfAMonth(aSYear, 7);
          FEndDateOfBound := EndOfAMonth(aEYear, 12);
        end;
    //года
    vptsYear:
      begin
        FStartDateOfBound := StartOfAYear(aSYear);
        FEndDateOfBound := EndOfAYear(aEYear);
      end;
  end;
  //пересчитываем значения календаря
  //хорошо было бы пересчитывать только если даты изменились, но при смене типа шкал тоже пересчитать надо,
  //а даты при этом не меняются :(
  if FvpGanttCalendar.HandleAllocated then
    FvpGanttCalendar.CalcScaleCount;
  //и нужно пересчитать интервалы
  RecalcIntervals;
  {$ifdef DBGGANTT}
  Form1.Debug('Start date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', StartDate));
  Form1.Debug('End date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', EndDate));
  Form1.Debug('Start interval date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', FStartIntervalDate));
  Form1.Debug('End interval date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', FEndIntervalDate));
  {$endif}
end;

{ Функция вычисления номера строки под курсором мыши
  @param YPos integer вертикальная координата мыши
  @return integer номер строки под курсором, или -1 если кликнули по заголвку или пустому месту
}
function TvpGantt.GetRowPosY(const YPos: integer): integer;
var
  aTH: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetRowPosXY');
  {$endif}
  {DONE Отработать правильное нажатие на титле, иначе ниже середины получаем 0 строку и соответственно
        переход на первую строку}
  aTH := GetTitleHeight;
  Result := Trunc((YPos + FVScrollPosition - GetTitleHeight ) / RowHeight);
  if (YPos < aTH) OR (Result>IntervalCount-1) then
    Result := -1;
  {TODO: Сделать обработку вычисления положения курсора на заголовке}
  //if thisTitle then
  //  DoTitleClick;
end;


constructor TvpGantt.Create(AOwner: TComponent);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.Create');
  {$endif}
  inherited Create(AOwner);
  TabStop := true;
  {$ifdef DBGGANTT}
  Form1.EL.Debug('TabStop %d', [Integer(TabStop)]);
  {$endif}

  FIntervals := TList.Create;

  //create vpGanttTasks
  FvpGanttTasks := TvpGanttTasks.Create(Self);
  FvpGanttTasks.Width := C_DEF_TASKS_WIDTH;
  FvpGanttTasks.Align := alLeft;
  FvpGanttTasks.OnClick := @DoClick;
  FvpGanttTasks.OnDblClick := @DoDblClick;

  //create splitter
  FSplitter := TSplitter.Create(Self);
  InsertControl(FSplitter);
  //FSplitter.ResizeStyle := rsPattern;
  FSplitter.Left := FvpGanttTasks.Left + FvpGanttTasks.Width + 1;
  FSplitter.Align := alLeft;
  FSplitter.Width := C_DEF_SPLITTER_WIDTH;

  //create vpGanttTasks
  FvpGanttCalendar := TvpGanttCalendar.Create(Self);
  FvpGanttCalendar.Align := alClient;
  FvpGanttCalendar.OnClick := @DoClick;
  FvpGanttCalendar.OnDblClick := @DoDblClick;

  BeginUpdate;

  Options := DefaultGanttOptions;

  Width := C_VPGANTT_WIDTH;
  Height := C_VPGANTT_HEIGHT;

  GridBorderWidth := C_DEF_BORDER_WIDTH;
  TitleColor := clBtnFace;
  TaskColor := clWindow;
  CalendarColor := clWindow;
  BorderColor := clActiveBorder;
  ScrollBars := ssAutoBoth;
  FRowHighlightColor :=  GetHighLightColor(clHighlight, C_ROW_HIGHLIGHT_VALUE);

  FPlanIntervalColor := clAqua;
  FFactIntervalColor := GetShadowColor(clHighlight, C_FACT_SHADOW_VALUE);
  FFreeIntervalColor := clLime;
  FExpiredIntervalColor := clRed;

  RowHeight := C_DEF_ROW_HEIGHT;
  MajorScaleHeight := C_DEF_ROW_HEIGHT;
  MinorScaleHeight := C_DEF_ROW_HEIGHT;

  FTitleFont := TFont.Create;
  FTitleFont.OnChange := @OnTitleFontChanged;
  FTitleFontIsDefault := True;
  with FTitleTextStyle do
    begin
      Alignment := taLeftJustify;
      Layout := tlCenter;
      SingleLine := true;
      Clipping := true;
      Wordbreak := false;
      Opaque := true;
      SystemFont := false;
      RightToLeft := false;
      EndEllipsis := true;
    end;

  FVScrollPosition := 0;
  FScrollBarWidth  := GetSystemMetrics(SM_CXVSCROLL) +
                      GetSystemMetrics(SM_SWSCROLLBARSPACING);
  FScrollBarHeight := GetSystemMetrics(SM_CYHSCROLL) +
                      GetSystemMetrics(SM_SWSCROLLBARSPACING);

  FFocusRow := -1;
  FMouseInterval := -1;
  FFocusColor := clBlack;

  TaskTitleCaption := RS_TITLE_TASKS;

  DateFormat := '';

  StartDate := Now;

  EndUpdate();
end;

destructor TvpGantt.Destroy;
var
  i: Integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.Destroy');
  {$endif}
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
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.AddInterval');
  {$endif}
  FIntervals.Add(AnInterval);
  if vpgMoveFocusToNewInterval in Options then
    SetFocusRow(FIntervals.Count-1);
  UpdateInterval(IntervalCount-1);
end;

procedure TvpGantt.InsertInterval(AnIndex: Integer; AnInterval: TvpInterval);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.InsertInterval');
  {$endif}
  FIntervals.Insert(AnIndex, AnInterval);
  if vpgMoveFocusToNewInterval in Options then
    SetFocusRow(AnIndex);
  UpdateInterval(AnIndex);
end;

procedure TvpGantt.DeleteInterval(AnIndex: Integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DeleteInterval');
  {$endif}
  if AnIndex>IntervalCount-1 then
    raise Exception.Create(RS_E_LIST_INDEX_OUT_OF_BOUNDS)
  else if AnIndex=IntervalCount-1 then
    SetFocusRow(AnIndex-1)
  else
    SetFocusRow(AnIndex);
  FIntervals.Delete(AnIndex);
  {DONE -o Vas Нужно проверить все интервалы и перестроить индекс даты.
  НЕ НАДО! проверять, пусть юзер сам обновляет, чтобы не сломать диапазон заданный,
  т.к. может быть удален самый длинный интервал}
  UpdateInterval;
end;

procedure TvpGantt.RemoveInterval(AnInterval: TvpInterval);
var
  AnIndex: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.RemoveInterval');
  {$endif}
  AnIndex := FIntervals.IndexOf(AnInterval);
  DeleteInterval(AnIndex);
end;

procedure TvpGantt.UpdateInterval(AnIndex: Integer = -1);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.UpdateInterval');
  {$endif}
  //изменяются даты, пересчитаем только в случае если календарь создан
  if AnIndex>=0 then
    UpdateBoundDates(TvpInterval(FIntervals[AnIndex]).StartDate,
                     TvpInterval(FIntervals[AnIndex]).FinishDate);
  VisualChange;
end;

procedure TvpGantt.RecalcIntervals;
var
  i:integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.RecalcIntervals');
  {$endif}
  BeginUpdate;
  if IntervalCount>0 then
    for i:=0 to IntervalCount-1 do
      TvpInterval(FIntervals[i]).UpdateBounds;
  EndUpdate();
end;

procedure TvpGantt.Clear;
var
  i:integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.Clear');
  {$endif}
  BeginUpdate;
  for i:=0 to IntervalCount-1 do
    TvpInterval(FIntervals[i]).Free;
  FIntervals.Clear;
  FStartDateOfBound := 0;
  FEndDateOfBound := 0;
  UpdateBoundDates(FStartDate, FEndDate);
  {TODO -o Vas: Подумать о сбросе горизонтального скроллинга в календаре}
  //if FvpGanttCalendar.HandleAllocated then
  //  FvpGanttCalendar.FHScrollPosition := 0;
  EndUpdate();
end;

procedure TvpGantt.BeginUpdate;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.BeginUpdate');
  {$endif}
  Inc(FUpdateCount);
end;

procedure TvpGantt.EndUpdate(aRefresh: boolean = true);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.EndUpdate');
  {$endif}
  Dec(FUpdateCount);
  if (FUpdateCount<=0) and aRefresh then
    begin
      FUpdateCount := 0;
      VisualChange;
    end;
end;

procedure TvpGantt.First;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.First');
  {$endif}
  SetFocusRow(0);
end;

procedure TvpGantt.Last;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.Last');
  {$endif}
  SetFocusRow(FIntervals.Count-1);
end;

procedure TvpGantt.MoveTo(ARow: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.MoveTo');
  {$endif}
  SetFocusRow(ARow);
end;

function TvpGantt.GetTitleHeight: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetTitleHeight');
  {$endif}
  Result := FMajorScaleHeight + FMinorScaleHeight;
end;

procedure TvpGantt.HideRowHintWindow;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.HideRowHintWindow');
  {$endif}
  Hint := FSavedHint;
  Application.CancelHint;
end;

procedure TvpGantt.DrawCellGrid(ACanvas: TCanvas; const aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawCellGrid');
  {$endif}
  //заливаем цветом бордюра
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := FBorderColor;
  ACanvas.FillRect(aRect);
end;

procedure TvpGantt.DrawCell(aRow: integer; ACanvas: TCanvas; const aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawCell');
  {$endif}
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(aRect);
  if aRow=FFocusRow then
    DrawHighlightRect(ACanvas, aRect);
end;


procedure TvpGantt.DrawThemedCell(ACanvas: TCanvas; const aRect: TRect);
var
  details: TThemedElementDetails;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawThemedCell');
  {$endif}
  //if gdPushed in aState then
  //  Details := ThemeServices.GetElementDetails(thHeaderItemPressed)
  //else if gdHot in aState then
  //  Details := ThemeServices.GetElementDetails(thHeaderItemHot)
  //else
    Details := ThemeServices.GetElementDetails(thHeaderItemNormal);
  ThemeServices.DrawElement(ACanvas.Handle, Details, aRect, nil);
end;

procedure TvpGantt.DrawFillRect(ACanvas: TCanvas; const aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawFillRect');
  {$endif}
  ACanvas.FillRect(aRect);
end;

procedure TvpGantt.DrawFocusRect(ACanvas: TCanvas; const aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TGantt.DrawFocusRect');
  // Draw focused cell if we have the focus
  Form1.EL.Debug('Focused %d', [Integer(Focused)]);
  {$endif}
  //дальше рисуем
  if Focused then
    begin
      //если фокус подсвечивать
      if (vpgFocusHighlight in Options) then
        begin
          ACanvas.Brush.Color := clHighlight;
          ACanvas.Font.Color := clHighlightText;
          ACanvas.FillRect(aRect);
        end;
      //if FUseXORFeatures then begin
      //  Canvas.SaveHandleState;
      //  OldFocusColor := FFocusColor;
      //  FFocusColor:= clBlack;//White not visible on White background
      //  OldPenMode:=Canvas.Pen.Mode;
      //  Canvas.Pen.Mode := pmXOR;
      //end;
      DrawRubberRect(ACanvas, aRect, FocusColor);
      //if FUseXORFeatures then begin
      //  Canvas.Pen.Mode := OldPenMode;
      //  Canvas.RestoreHandleState;
      //  FFocusColor := OldFocusColor;
      //end;
    end
  else
    //если рисовать фокус всегда, то независимо от фокуса надо нарисовать неактивным цветом фокус
    if (vpgFocusHighlight in Options) then
      begin
        ACanvas.Brush.Color := cl3DDkShadow;
        ACanvas.Font.Color := Font.Color;
        ACanvas.FillRect(aRect);
      end;
end;


procedure TvpGantt.DrawHighlightRect(ACanvas: TCanvas; const aRect: TRect);
var
  aOldBrush: TBrush;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawHighlightRect');
  {$endif}
  if vpgRowHighlight in Options then
    begin
      AOldBrush := Canvas.Brush;
      if Focused then
        ACanvas.Brush.Color := FRowHighlightColor
      else
        ACanvas.Brush.Color := cl3DShadow;
      ACanvas.FillRect(ARect);
      ACanvas.Brush := aOldBrush;
    end;
end;

procedure TvpGantt.DrawTitleGrid(ACanvas: TCanvas; const aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawTitleGrid');
  {$endif}
  if FGridBorderWidth>0 then
    begin
      //не рисуем обводку, если у нас используются стили
      if FTitleStyle = tsNative then
        Exit;
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Color := cl3DHilight;
      ACanvas.MoveTo(aRect.Right - 1, aRect.Top);
      ACanvas.LineTo(aRect.Left, aRect.Top);
      ACanvas.LineTo(aRect.Left, aRect.Bottom - 1);
      //если не используем tsStandart то будем рисовать рамку цветом cl3DShadow
      ACanvas.Pen.Color := cl3DShadow;
      if FTitleStyle = tsStandard then
        begin
          ACanvas.MoveTo(aRect.Left + 1, aRect.Bottom - 2);
          ACanvas.LineTo(aRect.Right - 2, aRect.Bottom - 2);
          ACanvas.LineTo(aRect.Right - 2, aRect.Top + 1);
          //Иначе внешний цвет деаем темнее
          ACanvas.Pen.Color := cl3DDKShadow;
        end;
      ACanvas.MoveTo(aRect.Left, aRect.Bottom - 1);
      ACanvas.LineTo(aRect.Right - 1, aRect.Bottom - 1);
      ACanvas.LineTo(aRect.Right - 1, aRect.Top);
    end;
end;

procedure TvpGantt.DrawTitleCell(ACanvas: TCanvas; const aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawTitleCell');
  {$endif}
  //рисуем бекграунд со стилями
  if FTitleStyle = tsNative then
    DrawThemedCell(ACanvas, aRect)
  else
    begin
      //заливаем
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := TitleColor;
      ACanvas.FillRect(aRect);
    end;
  //граница
  DrawTitleGrid(ACanvas, aRect);
end;

procedure TvpGantt.DrawTitleText(ACanvas: TCanvas; const aRect: TRect; aText: string;
  aAligment: TAlignment = taLeftJustify);
var
  aTextStyle: TTextStyle;
  textRect: TRect;
  aBrushStyle: TBrushStyle;
  textWidth: integer;
begin
  {DONE: -o Vas Попробовать сделать для первого диапазона не прокручивающуюся надпись  }
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawTitleText');
  {$endif}
  //рисуем названия
  aBrushStyle := ACanvas.Brush.Style;
  ACanvas.Brush.Style := bsClear;
  aTextStyle := FTitleTextStyle;
  aTextStyle.Alignment := aAligment;
  textRect := TRect.Create(aRect.Left + C_TITLE_TEXT_INDENT, aRect.Top, aRect.Right - C_TITLE_TEXT_INDENT, aRect.Bottom);
  //если не прокручивать надписи в заголовке, то надо вычислить область текста
  //относительно области рисования, а она у нас ClientRect
  if (vpgTitleTextNoScroll in Options) AND (textRect.Left<ClientRect.Left) then
    begin
      textWidth := Canvas.TextWidth(aText) + 2*C_TITLE_TEXT_INDENT;
      textRect.Left := ClientRect.Left + C_TITLE_TEXT_INDENT;
      if textRect.Width<textWidth then
        textRect.Left := textRect.Left - (textWidth - textRect.Width);
    end;
  //выводим текст
  ACanvas.TextRect(textRect, textRect.Left, textRect.Top, aText, aTextStyle);
  ACanvas.Brush.Style := aBrushStyle;
end;


end.
