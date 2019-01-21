unit vpGantt;

//{$define DBGINTERVAL}
{$define DBGGANTT}
{$define DBGGANTTTASKS}
//{$define DBGGANTTCALENDAR}

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
  C_ROW_HIGHLITE_VALUE = 50;

type
  TTitleStyle = (tsLazarus, tsStandard, tsNative);

  TvpgOption = (vpgDrawFocusSelected,  //рисовать всегда выделение
                vpgFocusHighlight,     //подсвечивать фокус, а не только рамку рисовать
                vpgMajorVertGrids,     //рисовать вертикальные линии мажорной шкалы
                vpgMinorVertGrids,     //рисовать вертикальные линии минорной шкалы
                vpgRowHighlight);      //рисовать подсветку всей строки

  TvpGanttOptions = set of TvpgOption;

  //vptsMinute - минуты,
  //vptsDecMinute - десятки минут (10..60),
  //vptsHour - часы,
  //vptsDay - дни,
  //vptsDayWeek - дни недели,
  //vptsWeek - недели (дата.год),
  //vptsWeekNum - номер недели,
  //vptsWeekNumPlain - название недели,
  //vptsMonth - месяцы,
  //vptsQuarter - кварталы,
  //vptsHalfYear - полугодие,
  //vptsYear - год
  TvpTimeScale = (vptsMinute,
                  vptsDecMinute,
                  vptsHour,
                  vptsDay,
                  vptsDayWeek,
                  vptsWeek,
                  vptsWeekNum,
                  vptsWeekNumPlain,
                  vptsMonth,
                  vptsQuarter,
                  vptsHalfYear,
                  vptsYear);

const
  SCROLL_PAGE_DEFAULT = 100;
  constCellPadding: byte = 3;
  constRubberSpace: byte = 2;
  DefaultGanttOptions = [vpgDrawFocusSelected, vpgFocusHighlight, vpgMajorVertGrids, vpgMinorVertGrids, vpgRowHighlight];

resourcestring
  RS_TITLE_TASKS = 'Задача';
  //Scale
  RS_E_MAJORSCALE_LOW = 'MajorScale should by higher than MinorScale';
  RS_E_MAJORSCALE_DIFF = 'MajorScale should by different from MinorScale';
  RS_E_MINORSCALE_HIGH = 'MinorScale should by lower than MajorScale';
  RS_E_MINORSCALE_DIFF = 'MinorScale should by different from MajorScale';
  //date
  RS_HOUR = 'ч.';
  RS_WEEK = 'Нед. ';
  RS_KW = 'Кв. ';


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
      FTasksHeight: integer; //нужно для хранения высоты области без/с скролом
      FTitleHeight: integer;
      FHScrollPosition: integer;
      //scrollbars
      FHSbVisible: boolean;
      //methods
      function CalcFocusRect(var aRect: TRect): TRect;
      function CalcRowRect(aRow: integer): TRect;
      procedure CalcTasksWidth;
      procedure CalcTasksHeight;
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
      procedure ClearCanvas;
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure DrawRows;
      procedure DrawEdges;
      procedure DrawFocusRect(aRow: integer; ARect: TRect);
      procedure DrawRow(aRow: integer);
      procedure DrawTitle;
      procedure GetSBVisibility(out HsbVisible: boolean);
      procedure GetSBRanges(const HsbVisible: boolean;
                    out HsbRange, HsbPage, HsbPos: Integer);
      procedure Paint; override;
      function  ScrollBarAutomatic(Which: TScrollStyle): boolean;
      function  ScrollBarIsVisible(Which:Integer): Boolean;
      procedure ScrollBarPosition(Which, Value: integer);
      procedure ScrollBarRange(Which:Integer; aRange,aPage,aPos: Integer);
      procedure ScrollBarShow(Which: Integer; aValue: boolean);
      procedure UpdateHorzScrollBar(const aVisible: boolean; const aRange,aPage,aPos: Integer);
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
      FMajorScale: TvpTimeScale;
      FMinorScale: TvpTimeScale;
      FMajorScaleCount: integer;
      FMinorScaleCount: integer;
      FMinorPerMajorScale: integer;
      FMajorScaleTitleRect: TRect;
      FMinorScaleTitleRect: TRect;
      FPixelePerMinorScale: integer;

      FStartIntervalDate: TDateTime;
      FEndIntervalDate: TDateTime;

      //scrollbars
      FHScrollPosition: integer;
      FVScrollPosition: integer;
      FVSbVisible: boolean;
      FHSbVisible: boolean;
      //methods
      procedure CalcCalendarHeight;
      procedure CalcCalendarWidth;
      function CalcFocusRect(ARect: TRect): TRect;
      function CalcRowRect(const aRow: integer): TRect;
      procedure CalcScaleCount;
      function GetMajorScaleHeight: integer;
      function GetMajorScaleWidth: integer;
      function GetMinorScaleHeight: integer;
      function GetMinorScaleWidth: integer;
      procedure UpdateSBVisibility;
      procedure UpdateSizes;
      //messages
      procedure WMSize(var Message: TLMSize);
        message LM_SIZE;
    protected
      procedure CalcScrollbarsRange;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure ClearCanvas;
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
      function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure DrawEdges;
      procedure DrawFocusRect(aRow: integer; aRect: TRect);
      procedure DrawMajorScale;
      procedure DrawMinorScale;
      procedure DrawRow(const aRow: integer);
      procedure DrawRows;
      procedure GetSBVisibility(out HsbVisible,VsbVisible:boolean);
      procedure GetSBRanges(const HsbVisible,VsbVisible: boolean;
                    out HsbRange,VsbRange,HsbPage,VsbPage,HsbPos,VsbPos:Integer);
      procedure Paint; override;
      procedure ScrollBarRange(Which:Integer; aRange,aPage,aPos: Integer);
      procedure ScrollBarPosition(Which, Value: integer);
      function  ScrollBarIsVisible(Which:Integer): Boolean;
      procedure ScrollBarPage(Which: Integer; aPage: Integer);
      procedure ScrollBarShow(Which: Integer; aValue: boolean);
      function  ScrollBarAutomatic(Which: TScrollStyle): boolean;
      procedure UpdateHorzScrollBar(const aVisible: boolean; const aRange,aPage,aPos: Integer);
      procedure UpdateVertScrollbar(const aVisible: boolean; const aRange,aPage,aPos: Integer);
      procedure UpdateIntervalDates(AStartDate, AEndDate: TDate);
      procedure VisualChange; virtual;
      procedure WMHScroll(var message : TLMHScroll); message LM_HSCROLL;
      procedure WMVScroll(var message : TLMVScroll); message LM_VSCROLL;
      procedure WMLButtonDown(var message: TLMLButtonDown); message LM_LBUTTONDOWN;
    public
      constructor Create(AOwner: TvpGantt);  reintroduce;
      destructor Destroy; override;
  end;

  {TvpGantt}
  TvpGantt = class(TCustomControl)
    private
      FEndDate: TDate;
      FStartDate: TDate;
      FvpGanttOptions: TvpGanttOptions;
      FvpGanttTasks: TvpGanttTasks;
      FvpGanttCalendar: TvpGanttCalendar;
      FSplitter: TSplitter;
      FScrollBars: TScrollStyle;
      FGanttFocused: boolean;

      FIntervals: TList;

      FUpdateCount: integer;

      FRowHeight: integer;
      FMajorScaleHeight: integer;
      FMinorScaleHeight: integer;
      FIntervalsHeight: integer;
      FGanttBorderWidth: integer;
      FBorderColor: TColor;
      FCalendarColor: TColor;
      FFocusColor: TColor;
      FRowHighlightColor: TColor;
      FTaskColor: TColor;
      FTitleColor: TColor;
      FTaskTitleCaption: TCaption;
      FTitleFont: TFont;
      FTitleFontIsDefault: boolean;
      FTitleStyle: TTitleStyle;
      FTitleTextStyle: TTextStyle;
      FFocusRow: integer;
      //methods
      procedure ClearDates;
      function GetIntervalCount: Integer;
      function GetInterval(AnIndex: Integer): TvpInterval;
      function GetBorderWidth: integer;
      function GetMajorScale: TvpTimeScale;
      function GetMinorScale: TvpTimeScale;
      function GetPixelPerMinorScale: integer;
      function GetTaskTitleCaption: TCaption;
      procedure OnTitleFontChanged(Sender: TObject);
      function OptionsIsStored: Boolean;
      procedure SetEndDate(AValue: TDate);
      procedure SetFocusColor(AValue: TColor);
      procedure SetMajorScale(AValue: TvpTimeScale);
      procedure SetMajorScaleHeight(AValue: integer);
      procedure SetMinorScale(AValue: TvpTimeScale);
      procedure SetMinorScaleHeight(AValue: integer);
      procedure SetOptions(AValue: TvpGanttOptions);
      procedure SetPixelPerMinorScale(AValue: integer);
      procedure SetRowHeight(AValue: integer);
      procedure SetGanttBorderWidth(AValue: integer);
      procedure SetBorderColor(AValue: TColor);
      procedure SetStartDate(AValue: TDate);
      procedure SetTaskColor(AValue: TColor);
      procedure SetCalendarColor(AValue: TColor);
      procedure SetScrollBars(const AValue: TScrollStyle);
      procedure SetTaskTitleCaption(AValue: TCaption);
      procedure SetTitleColor(AValue: TColor);
      procedure SetTitleFont(const AValue: TFont);
      procedure SetTitleStyle(const AValue: TTitleStyle);
      procedure SetFocus; override;
      procedure VisualChange;
      procedure WMKillFocus(var message: TLMKillFocus); message LM_KILLFOCUS;
    protected
      procedure CalcIntervalsHeight;
      function GetIntervalsHeight: integer; //высота всех интервалов
      function GetFocusRow: integer;
      function GetMajorScaleHeight: integer;
      function GetMinorScaleHeight: integer;
      function GetRowPosY(const YPos: integer): integer;
      function GetTitleHeight: integer;
      procedure DrawThemedCell(ACanvas: TCanvas; aRect: TRect);
      procedure DrawFillRect(ACanvas:TCanvas; aRect: TRect);
      procedure DrawHighlightRect(ACanvas: TCanvas; ARect: TRect);
      procedure DrawTitleGrid(ACanvas: TCanvas; aRect: TRect);
      procedure DrawTitleCell(ACanvas: TCanvas; aRect: TRect);
      procedure DrawTitleText(ACanvas: TCanvas; aRect: TRect; aText: string; aAligment: TAlignment = taLeftJustify);
      function FixScrollPosition(const ARange, APage, APos: integer): integer;
      procedure FontChanged(Sender: TObject); override;
      procedure Paint; override;
      procedure RepaintFocus(aFocusRow: integer);
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
      procedure UpdateInterval(AUpdateDate: boolean = false);

      procedure BeginUpdate;
      procedure EndUpdate(aRefresh: boolean = true);

      procedure First;
      procedure Last;
      procedure MoveTo(ARow: integer);
      property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;
    published
      //standart TwinControl property
      property Align;
      property BorderStyle;
      property BorderWidth;
      //custom property
      property BorderColor: TColor read FBorderColor write SetBorderColor default clActiveBorder;
      property EndDate: TDate read FEndDate write SetEndDate;
      property GanttBorderWidth: Integer read FGanttBorderWidth write SetGanttBorderWidth default 1;
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
      //TODO Move this properties to child control OR stay this HOW RULES?
      property TaskColor: TColor read FTaskColor write SetTaskColor default clWindow;
      property TitleColor: TColor read FTitleColor write SetTitleColor default clBtnFace;
      property TaskTitleCaption: TCaption read GetTaskTitleCaption write SetTaskTitleCaption;
      property CalendarColor:TColor read FCalendarColor write SetCalendarColor default clWindow;
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

implementation

uses Unit1;

{$if Defined(DBGGANTT) OR Defined(DBGGANTTTASKS) OR Defined(DBGGANTTCALENDAR)}

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

{
  Процедура прорисовки квадратика активного фокуса в окне.
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

{
  Получаем рабочий канвас, если он не существует или =0 то получаем указатель
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

{
  Освобождаем рабочую канву
  @param Canvas TCanvas нужная канва
}
procedure FreeWorkingCanvas(canvas: TCanvas);
begin
  ReleaseDC(0, Canvas.Handle);
  Canvas.Free;
end;

{
  Функция определения количества временных интервалов между двумя датами
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

{
  Функция определения количества временных интервалов между двумя датами с
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

{
  Функция возвращает название временной шкалы
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
      Result := ShortDayNames[DayOfWeek(D)];
    vptsWeek:
      Result := IntToStr(Day) + '.' + IntToStr(Month);
    vptsWeekNum:
      Result := RS_WEEK + IntToStr(WeekOfTheYear(D));
    vptsWeekNumPlain:
      Result := IntToStr(WeekOfTheYear(D));
    vptsMonth:
      Result := LongMonthNames[Month] + ' ' + IntToStr(Year);
    vptsQuarter:
      Result := RS_KW + IntToStr((Month) div 3 + 1);
    vptsHalfYear:
      Result := IntToStr((Month) div 6 + 1);
    vptsYear:
      Result := IntToStr(Year);
  end;
end;

{
  Функция сдвига времени на нужный интервал. Будеи использовать для получения даты
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


{ TvpInterval }

constructor TvpInterval.Create(AvpGantt: TvpGantt);
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpInterval.Create');
  {$endif}
  FvpGantt := AvpGantt;
  //initialize
end;

destructor TvpInterval.Destroy;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpInterval.Destroy');
  {$endif}
  inherited Destroy;
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

function TvpGanttCalendar.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.ScrollBarAutomatic');
  {$endif}
  result:=false;
  if (Which=ssVertical)or(Which=ssHorizontal) then begin
    if Which=ssVertical then Which:=ssAutoVertical
    else Which:=ssAutoHorizontal;
    Result:= FvpGantt.ScrollBars in [Which, ssAutoBoth];
  end;
end;

function TvpGanttCalendar.CalcRowRect(const aRow: integer): TRect;
var
  aStart, aRowBorder: integer;
  tmpRect: TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttCalendar.CalcRowRect');
  {$EndIf}
  aRowBorder := FvpGantt.GetBorderWidth;
  // -1 потому что рисуем обводку по контуру календаря
  aStart := FvpGantt.MajorScaleHeight + FvpGantt.MinorScaleHeight +
            (FvpGantt.RowHeight + aRowBorder) * aRow - 1;
  //находим всю длину строки
  tmpRect := Rect(0 - FHScrollPosition, aStart,
                 FCalendarWidth + aRowBorder - FHScrollPosition  - 1, aStart + FvpGantt.RowHeight + aRowBorder);
  //обрезаем невидимые части
  IntersectRect(Result, tmpRect, ClientRect);
end;

procedure TvpGanttCalendar.CalcScaleCount;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcScale');
  {$endif}
  if (FStartIntervalDate=0) AND (FEndIntervalDate=0) then
    Exit;
  FMajorScaleCount := Round(UnitsBetweenDates(FStartIntervalDate, FEndIntervalDate, FMajorScale));
  FMinorScaleCount := Round(UnitsBetweenDates(FStartIntervalDate, FEndIntervalDate, FMinorScale));
  //целочисленное деление только для того чтобы компилятор не ругался
  FMinorPerMajorScale := FMinorScaleCount div FMajorScaleCount;
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
  FCalendarHeight := FvpGantt.GetIntervalsHeight;
end;

procedure TvpGanttCalendar.CalcCalendarWidth;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.CalcCalendarWidth');
  {$endif}
  FCalendarWidth := FMinorScaleCount * FPixelePerMinorScale;
end;

function TvpGanttCalendar.CalcFocusRect(ARect: TRect): TRect;
begin
  {$ifdef TvpGanttCalendar}
  Form1.Debug('TvpGanttCalendar.CalcFocusRect');
  {$endif}
  Result := aRect;
  Result.Left := Max(Result.Left, ClientRect.Left);
  Result.Right := Min(Result.Right, ClientRect.Right);
  inc(Result.Left);
  //InflateRect(Result, -2, -2);
  inc(Result.Top, 1);
end;

function TvpGanttCalendar.GetMajorScaleHeight: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetMajorScaleHeight');
  {$endif}
  Result := FMajorScaleTitleRect.Height;
end;

function TvpGanttCalendar.GetMajorScaleWidth: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.GetMajorScaleWidth');
  {$endif}
  Result := FPixelePerMinorScale * FMinorPerMajorScale;
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
  CalcCalendarHeight;
  CalcCalendarWidth;
  CalcScrollbarsRange;
end;

{
  Будем использовать процедуру всегда:
    1) при смене даты
    2) при смене любой из шкал
    3) при измненении интервалов
}
procedure TvpGanttCalendar.UpdateIntervalDates(AStartDate, AEndDate: TDate);
var
  aSYear, aSMonth, aSDay: word;
  aEYear, aEMonth, aEDay: word;
begin
  {$ifdef DBGGANTT}
  Form1.EL.Debug('TvpGanttCalendar.UpdateIntervalDates %s %s', [DateToStr(AStartDate), DateToStr(AEndDate)]);
  {$endif}
  //если текущая дата меньше устанавливаемой, то пересчитываем
  AStartDate := Min(AStartDate, FvpGantt.StartDate);
  DecodeDate(AStartDate, aSYear, aSMonth, aSDay);
  //а вдруг как-то передадим дату начала > даты окончания, не порядок
  AEndDate := Max(AEndDate, FvpGantt.EndDate);
  if AEndDate<AStartDate then
    AEndDate := AStartDate;
  DecodeDate(AEndDate, aEYear, aEMonth, aEDay);
  case FvpGantt.MajorScale of
    //берем начало дня
    //концом диапазона будем считать день в 23:59:59
    //минуты быть не могут, но возьмем и их
    vptsMinute, vptsDecMinute, vptsHour, vptsDay, vptsDayWeek:
      begin
       FStartIntervalDate := StartOfADay(aSYear, aSMonth, aSDay);
       FEndIntervalDate := EndOfADay(aEYear, aEMonth, aEDay);
      end;
    //недели
    vptsWeek, vptsWeekNum, vptsWeekNumPlain:
      begin
        FStartIntervalDate := StartOfAWeek(aSYear, aSMonth, aSDay);
        FEndIntervalDate := EndOfAWeek(aEYear, aEMonth, aEDay);
      end;
    //месяц
    vptsMonth:
      begin
        FStartIntervalDate := StartOfAMonth(aSYear, aSMonth);
        FEndIntervalDate := EndOfAMonth(aEYear, aEMonth);
      end;
    //квартал
    vptsQuarter:
      begin
        case aSMonth of
          1..3: FStartIntervalDate := StartOfAMonth(aSYear, 1);
          4..6: FStartIntervalDate := StartOfAMonth(aSYear, 4);
          7..9: FStartIntervalDate := StartOfAMonth(aSYear, 7);
          10..12: FStartIntervalDate := StartOfAMonth(aSYear, 10);
        end;
        case aSMonth of
          1..3: FEndIntervalDate := EndOfAMonth(aEYear, 3);
          4..6: FEndIntervalDate := EndOfAMonth(aEYear, 6);
          7..9: FEndIntervalDate := EndOfAMonth(aEYear, 9);
          10..12: FEndIntervalDate := EndOfAMonth(aEYear, 12);
        end;
      end;
    //полугодия
    vptsHalfYear:
      if aSMonth<7 then
        begin
          FStartIntervalDate := StartOfAMonth(aSYear, 1);
          FEndIntervalDate := EndOfAMonth(aEYear, 6);
        end
      else
        begin
          FStartIntervalDate := StartOfAMonth(aSYear, 7);
          FEndIntervalDate := EndOfAMonth(aEYear, 12);
        end;
    //года
    vptsYear:
      begin
        FStartIntervalDate := StartOfAYear(aSYear);
        FEndIntervalDate := EndOfAYear(aEYear);
      end;
  end;
  {$ifdef DBGGANTT}
  Form1.Debug('Start date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', FvpGantt.StartDate));
  Form1.Debug('End date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', FvpGantt.EndDate));
  Form1.Debug('Start interval date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', FStartIntervalDate));
  Form1.Debug('End interval date time ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', FEndIntervalDate));
  {$endif}
  CalcScaleCount;
  VisualChange;
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
  aPos, maxPos: Integer;
  ScrollInfo: TScrollInfo;
  aCode: Smallint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.WMHScroll');
  {$endif}
  if not HandleAllocated then
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
  aPos, maxPos: Integer;
  ScrollInfo: TScrollInfo;
  aCode: Smallint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.WMVScroll');
  {$endif}
  if not HandleAllocated then
    exit;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_POS;
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
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

  ScrollBarPosition(SB_VERT, aPos);
  FVScrollPosition := aPos;

  {$ifdef DBGGANTTCALENDAR}
  //Form1.Debug(Format('FVScrollPosition %d', [FVScrollPosition]));
  {$endif}

  Invalidate;
end;

procedure TvpGanttCalendar.WMLButtonDown(var message: TLMLButtonDown);
var
  aRow: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.WMLButtonDown');
  Form1.EL.Debug('x %d y %d',[message.XPos, message.YPos]);
  {$endif}
  inherited;
  //TODO Вычесть значение прокрутки по Y
  aRow := FvpGantt.GetRowPosY(message.YPos);
  if aRow>-1 then
    FvpGantt.SetFocusRow(aRow);
  FvpGantt.SetFocus;
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
  Canvas.Brush.Color := FvpGantt.CalendarColor;
  Canvas.FillRect(ClientRect);
end;

function TvpGanttCalendar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DoMouseWheel');
  {$endif}
  Form1.ShowParam(IntToStr(WheelDelta));
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
      FvpGantt.SelectNextRow(1);
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
      FvpGantt.SelectNextRow(-1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

procedure TvpGanttCalendar.DrawEdges;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawEdges');
  {$endif}
  //clear canvas
  Canvas.Brush.Color := FvpGantt.FTaskColor;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clActiveBorder;
  Canvas.Rectangle(ClientRect);
end;

procedure TvpGanttCalendar.DrawFocusRect(aRow: integer; aRect: TRect);
begin
  {$ifdef TvpGanttCalendar}
  Form1.Debug('TvpGanttCalendar.DrawFocusRect');
  // Draw focused cell if we have the focus
  Form1.EL.Debug('Focused %d', [Integer(Focused)]);
  {$endif}
  if FvpGantt.FGanttFocused then
    FvpGantt.DrawHighlightRect(Canvas, aRect)
  else if (vpgDrawFocusSelected in FvpGantt.Options) then
    begin
      Canvas.Brush.Color := cl3DShadow;
      Canvas.Font.Color := Font.Color;
      Canvas.FillRect(aRect);
    end;
  //if FUseXORFeatures then begin
  //  Canvas.SaveHandleState;
  //  OldFocusColor := FFocusColor;
  //  FFocusColor:= clBlack;//White not visible on White background
  //  OldPenMode:=Canvas.Pen.Mode;
  //  Canvas.Pen.Mode := pmXOR;
  //end;
  //DrawRubberRect(Canvas, aRect, FvpGantt.FocusColor);
  //if FUseXORFeatures then begin
  //  Canvas.Pen.Mode := OldPenMode;
  //  Canvas.RestoreHandleState;
  //  FFocusColor := OldFocusColor;
  //end;
end;

procedure TvpGanttCalendar.DrawMajorScale;
var
  aRect: TRect;
  aScaleName: string;
  i: integer;
  aStartRange, aStopRange: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawMajorScale');
  {$endif}
  //считаем кол-во минорных областей в одной мажорной области
  //берем для начала всю клиентскую область
  //длину вычисляем, высота будет равна заданной пользователем
  aRect := Rect(0, 0, GetMajorScaleWidth, FvpGantt.MajorScaleHeight);
  //считаем кол-во на которые прокручен скролл горизонтальный
  aStartRange := Max(0, FHScrollPosition div GetMajorScaleWidth - 1);
  //считаем кол-во видимых диапазонов
  aStopRange := Min(((FHScrollPosition + ClientWidth) div GetMajorScaleWidth) + 1, FMajorScaleCount);
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('DrawMajorStartStopRange %d %d', [aStartRange, aStopRange]);
  {$endif}
  //сдвигаем на горизонтальный скролинг, по вертикали двигать не будем заголовки
  //и на кол-во прокрученных диспазонов - 1
  OffsetRect(aRect, GetMajorScaleWidth * aStartRange - FHScrollPosition , 0);
  //перебираем все и рисуем
  for i:=aStartRange to aStopRange-1 do
    begin
      aScaleName := GetTimeScaleName(FMajorScale, IncTime(FStartIntervalDate, FMajorScale, i));
      //рисуем
      FvpGantt.DrawTitleCell(Canvas, aRect);
      FvpGantt.DrawTitleText(Canvas, aRect, aScaleName, taLeftJustify);
      //сдвигаем область
      OffsetRect(aRect, GetMajorScaleWidth, 0);
    end;
end;

procedure TvpGanttCalendar.DrawMinorScale;
var
  aRect: TRect;
  i: integer;
  aScaleName: string;
  aStartRange, aStopRange: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawMinorScale');
  {$endif}
  //находим область первого диапазона
  aRect := Rect(0, FvpGantt.MajorScaleHeight + 1,
                FPixelePerMinorScale,
                FvpGantt.MajorScaleHeight + FvpGantt.MinorScaleHeight);
  //считаем кол-во на которые прокручен скролл горизонтальный
  aStartRange := Max(0, FHScrollPosition div GetMinorScaleWidth - 1);
  //считаем кол-во видимых диапазонов
  aStopRange := Min(((FHScrollPosition + ClientWidth) div GetMinorScaleWidth) + 1, FMinorScaleCount);
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('DrawMinorStartStopRange %d %d', [aStartRange, aStopRange]);
  {$endif}
  //сдвигаем на горизонтальный скролинг, по вертикали двигать не будем заголовки
  OffsetRect(aRect, GetMinorScaleWidth * aStartRange - FHScrollPosition , 0);
  for i:=aStartRange to aStopRange-1 do
    begin
      aScaleName := GetTimeScaleName(FMinorScale, IncTime(FStartIntervalDate, FMinorScale, i));
      //рисуем
      FvpGantt.DrawTitleCell(Canvas, aRect);
      FvpGantt.DrawTitleText(Canvas, aRect, aScaleName, taCenter);
      //сдвигаем область
      OffsetRect(aRect, FPixelePerMinorScale, 0);
    end;
end;

procedure TvpGanttCalendar.DrawRow(const aRow: integer);
var
  rRect, fRect: TRect;
  i: integer;
  aBorderWidth: integer;
  aMinorScaleWidth, aMajorScaleWidth: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawRow');
  {$endif}
  if not Assigned(FvpGantt) then
    Exit;
  rRect := CalcRowRect(aRow);
  //получаем ширину бордюра и шкал
  aBorderWidth := FvpGantt.GetBorderWidth;
  aMajorScaleWidth := GetMajorScaleWidth;
  aMinorScaleWidth := GetMinorScaleWidth;
  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('rRect.Top %d  rRect.Height %d', [rRect.top, rRect.Height]);
  {$endif}
  if rRect.Height>0 then
    begin
      {$ifdef DBGGANTTCALENDAR}
      Form1.EL.Debug('Drawing row %d', [aRow]);
      {$endif}
      //если фокус, то зальем строку
      if aRow=FvpGantt.GetFocusRow then
        begin
          //если строку подсвечивать
          fRect := CalcFocusRect(rRect);
          FvpGantt.DrawHighlightRect(Canvas, fRect);
          DrawFocusRect(aRow, fRect);
        end;
      //устанавливаем шриф и рисуем
      Canvas.Font := Font;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := FvpGantt.BorderColor;
      Canvas.MoveTo(rRect.Left, rRect.Bottom);
      Canvas.LineTo(rRect.Right, rRect.Bottom);
      Canvas.LineTo(rRect.Right, rRect.Top);
      //вертикальная разметка
      Canvas.Pen.Style := psDot;
      if vpgMinorVertGrids in FvpGantt.Options then
        begin
          for i:=1 to FMinorScaleCount-1 do
            Canvas.Line(aMinorScaleWidth*i - aBorderWidth - FHScrollPosition,
                        rRect.Top,
                        aMinorScaleWidth*i - aBorderWidth - FHScrollPosition,
                        rRect.Bottom)
        end
      else if vpgMajorVertGrids in FvpGantt.Options then
        begin
          for i:=1 to FMajorScaleCount-1 do
            Canvas.Line(aMajorScaleWidth*i - aBorderWidth - FHScrollPosition,
                        rRect.Top,
                        aMajorScaleWidth*i - aBorderWidth - FHScrollPosition,
                        rRect.Bottom);
        end;
      ////считаем сдвиг и выводим текст
      //rRect.Top := Round(rRect.Top + (rRect.Height - Canvas.TextHeight('A'))/2);
      //Canvas.TextRect(rRect, rRect.Left + constCellPadding, rRect.Top, FvpGantt.Interval[aRow].Name);
    end;
end;

procedure TvpGanttCalendar.DrawRows;
var
  i: integer;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.DrawRows');
  {$endif}
  for i:=0 to FvpGantt.IntervalCount-1 do
    DrawRow(i);
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
  AutoVert := ScrollBarAutomatic(ssVertical);
  AutoHorz := ScrollBarAutomatic(ssHorizontal);

  //// get client bounds free of bars
  ClientW  := ClientWidth;
  ClientH  := ClientHeight;
  BarW := GetSystemMetrics(SM_CXVSCROLL) +
          GetSystemMetrics(SM_SWSCROLLBARSPACING);
  if ScrollBarIsVisible(SB_VERT) then
    ClientW := ClientW + BarW;
  BarH := GetSystemMetrics(SM_CYHSCROLL) +
          GetSystemMetrics(SM_SWSCROLLBARSPACING);
  if ScrollBarIsVisible(SB_HORZ) then
    ClientH := ClientH + BarH;

  // first find out if scrollbars need to be visible by
  // comparing against client bounds free of bars
  HsbVisible := (FvpGantt.FScrollBars in [ssHorizontal, ssBoth]) or
                (AutoHorz and (FCalendarWidth>ClientW));

  VsbVisible := (FvpGantt.FScrollBars in [ssVertical, ssBoth]) or
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
      VsbRange := FCalendarHeight - (GetMajorScaleHeight + GetMinorScaleWidth);
      VsbPage := ClientHeight - (GetMajorScaleHeight + GetMinorScaleWidth);
    end;

  {$ifdef DBGGANTTCALENDAR}
  Form1.EL.Debug('GetSBRanges: HRange=%d HPage=%d HPos=%d VRange=%d VPage=%d VPos=%d',
    [HSbRange, HsbPage, HsbPos, VsbRange, VsbPage, VsbPos]);
  {$endif}
end;

procedure TvpGanttCalendar.Paint;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.Paint');
  {$endif}
  ClearCanvas;
  DrawEdges;
  DrawMajorScale;
  DrawMinorScale;
  DrawRows;
end;

constructor TvpGanttCalendar.Create(AOwner: TvpGantt);
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.Create');
  {$endif}
  inherited create(AOwner);
  FvpGantt := AOwner;
  FHScrollPosition := 0;
  FVScrollPosition := 0;

  FMajorScale := vptsMonth;
  FMinorScale := vptsDay;
  FPixelePerMinorScale := C_DEF_PIXEL_PER_MINOR;
end;

destructor TvpGanttCalendar.Destroy;
begin
  {$ifdef DBGGANTTCALENDAR}
  Form1.Debug('TvpGanttCalendar.Destroy');
  {$endif}
  inherited Destroy;
end;

{ TvpGanttTasks }

procedure TvpGanttTasks.UpdateSizes;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.UpdateSizes');
  {$endif}
  if HandleAllocated then
    begin
      CalcTasksHeight;
      CalcTasksWidth;
      CalcScrollbarsRange;
    end;
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
  CalcTitleHeight;
  result := Rect(0, 0, ClientWidth, FTitleHeight);
end;

procedure TvpGanttTasks.WMSize(var Message: TLMSize);
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.WMSize');
  {$endif}
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
  Canvas.Brush.Color := FvpGantt.FTaskColor;
  Canvas.FillRect(ClientRect);
end;

function TvpGanttTasks.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DoMouseWheel');
  {$endif}
  Form1.ShowParam(IntToStr(WheelDelta));
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
      FvpGantt.SelectNextRow(1);
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
      FvpGantt.SelectNextRow(-1);
      Result := True; // handled, no further scrolling by the widgetset
    end;
end;

function TvpGanttTasks.CalcFocusRect(var aRect: TRect): TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcFocusRect');
  {$endif}
  Result := aRect;
  Result.Left := 0;
  Result.Right := Min(Result.Right, ClientWidth);
  inc(Result.Left);
  inc(Result.Top);
end;

function TvpGanttTasks.CalcRowRect(aRow: integer): TRect;
var
  aStart, aRowBorder: integer;
  tmpRect: TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcRowRect');
  {$EndIf}
  aRowBorder := FvpGantt.GetBorderWidth;
  {DONE Рассчитать правую границу в соответствии с ClientWidth
        было FTasksWidth + aRowBorder - FHScrollPosition  стало ClientWidth}
  // -1 потому что рисуем обводку по контуру списка задач
  aStart := FTitleHeight +
            (FvpGantt.RowHeight + aRowBorder) * aRow - 1;
  tmpRect := Rect(0 - FHScrollPosition, aStart,
                  ClientWidth - 1, aStart + FvpGantt.RowHeight + aRowBorder);
  //обрезаем невидимые части
  IntersectRect(Result, tmpRect, ClientRect);
end;

procedure TvpGanttTasks.CalcTasksWidth;
var
  i, aBorder: integer;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalculateTasksWidth');
  {$EndIf}
  FTasksWidth := 0;
  aBorder := FvpGantt.GetBorderWidth;
  for i:=0 to FvpGantt.IntervalCount - 1 do
    FTasksWidth := Max(FTasksWidth, Canvas.TextWidth(FvpGantt.Interval[i].FName) );
  FTasksWidth := Max(FTasksWidth + 2*constCellPadding + aBorder, ClientWidth);
end;

procedure TvpGanttTasks.CalcTasksHeight;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcTasksHeight');
  {$EndIf}
  FTasksHeight := FvpGantt.FIntervalsHeight;
end;

procedure TvpGanttTasks.CalcTitleHeight;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.CalcTitleHeight');
  {$endif}
  if not Assigned(FvpGantt) then
    Exit;
  FTitleHeight := FvpGantt.GetTitleHeight;
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
  for i:=0 to FvpGantt.IntervalCount-1 do
    DrawRow(i);
end;

procedure TvpGanttTasks.DrawEdges;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DrawEdges');
  {$endif}
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clActiveBorder;
  Canvas.Rectangle(ClientRect);
  // Canvas.FrameRect(ClientRect);
end;

procedure TvpGanttTasks.DrawFocusRect(aRow: integer; aRect: TRect);
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DrawFocusRect');
  // Draw focused cell if we have the focus
  Form1.EL.Debug('Focused %d', [Integer(Focused)]);
  {$endif}
  //дальше рисуем
  if FvpGantt.FGanttFocused then
    begin
      //если фокус подсвечивать
      if (vpgFocusHighlight in FvpGantt.Options) then
        begin
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
    end
  else
    //если рисовать фокус всегда, то независимо от фокуса надо нарисовать неактивным цветом фокус
    if (vpgFocusHighlight in FvpGantt.Options) then
      begin
        Canvas.Brush.Color := cl3DDkShadow;
        Canvas.Font.Color := Font.Color;
        Canvas.FillRect(aRect);
      end;
end;

procedure TvpGanttTasks.DrawRow(aRow: integer);
var
  rRect, fRect: TRect;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.DrawRow');
  {$endif}
  if not Assigned(FvpGantt) then Exit;
  rRect := CalcRowRect(aRow);
  {$ifdef DBGGANTTTASKS}
  Form1.EL.Debug('rRect.Left %d rRect.Top %d rRect.Right %d rRect.Bottom %d',
                  [rRect.Left, rRect.Top, rRect.Right, rRect.Bottom]);
  Form1.EL.Debug('FHScrollPosition %d, ClientWidth %d, FTasksWidth %d',
                  [FHScrollPosition, ClientWidth, FTasksWidth]);
  {$endif}
  if rRect.Height>0 then
    begin
      {$ifdef DBGGANTTTASKS}
      Form1.EL.Debug('Drawing row %d', [aRow]);
      {$endif}
      Canvas.Font := Font;
      //focus
      if aRow=FvpGantt.GetFocusRow then
        begin
          //если строку подсвечивать
          fRect := CalcFocusRect(rRect);
          FvpGantt.DrawHighlightRect(Canvas, fRect);
          DrawFocusRect(aRow, fRect);
        end;
      //бордюр и текст
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := FvpGantt.BorderColor;
      Canvas.MoveTo(rRect.Left, rRect.Bottom);
      Canvas.LineTo(rRect.Right, rRect.Bottom);
      Canvas.LineTo(rRect.Right, rRect.Top);
      //считаем сдвиг и выводим текст
      rRect.Top := Round(rRect.Top + (rRect.Height - Canvas.TextHeight('A'))/2);
      InflateRect(rRect, -1, -1);
      Canvas.TextRect(rRect, rRect.Left + constCellPadding - FHScrollPosition, rRect.Top, FvpGantt.Interval[aRow].Name);
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
  titCaption := PChar(FvpGantt.TaskTitleCaption);
  if Assigned(FvpGantt) then
    Canvas.Font.Assign(FvpGantt.TitleFont)
  else
    Canvas.Font := Font;
  FvpGantt.DrawTitleCell(Canvas, titRect);
  FvpGantt.DrawTitleText(Canvas, titRect, titCaption, taCenter);
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

procedure TvpGanttTasks.Paint;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.Paint');
  {$endif}
  ClearCanvas;
  DrawEdges;
  DrawTitle;
  DrawRows;
end;

function TvpGanttTasks.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.ScrollBarAutomatic');
  {$EndIf}
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

procedure TvpGanttTasks.WMLButtonDown(var message: TLMLButtonDown);
var
  aRow: integer;
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.WMLButtonDown');
  Form1.EL.Debug('x %d y %d',[message.XPos, message.YPos]);
  {$endif}
  inherited;
  //TODO Вычесть значение прокрутки по Y
  aRow := FvpGantt.GetRowPosY(message.YPos);
  if aRow>-1 then
    FvpGantt.SetFocusRow(aRow);
  FvpGantt.SetFocus;
end;

constructor TvpGanttTasks.Create(AOwner: TvpGantt);
begin
  {$ifdef DBGGANTTTASKS}
  Form1.Debug('TvpGanttTasks.Create');
  {$endif}
  inherited create(AOwner);
  FvpGantt := AOwner;
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

procedure TvpGantt.ClearDates;
begin
  FStartDate := 0;
  FEndDate := 0;
end;

procedure TvpGantt.CalcIntervalsHeight;
var
  i: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.CalcIntervalsHeight');
  {$EndIf}
  FIntervalsHeight := IntervalCount * RowHeight;
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
      if FvpGanttCalendar.HandleAllocated then
        FvpGanttCalendar.UpdateIntervalDates(FStartDate, FEndDate);
    end;
end;

procedure TvpGantt.SetFocusColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetFocusColor');
  {$endif}
  if FFocusColor = AValue then Exit;
  FFocusColor := AValue;
  //TODO Сделать обновление области с фокусом
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
    raise Exception.Create(RS_E_MAJORSCALE_DIFF)
  else
    begin
      FvpGanttCalendar.FMajorScale := AValue;
      FvpGanttCalendar.UpdateIntervalDates(FStartDate, FEndDate);
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
    raise Exception.Create(RS_E_MINORSCALE_DIFF)
  else
    begin
      FvpGanttCalendar.FMinorScale := AValue;
      FvpGanttCalendar.UpdateIntervalDates(FStartDate, FEndDate);
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

procedure TvpGantt.SetRowHeight(AValue: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetRowHeight');
  {$endif}
  if FRowHeight = AValue then Exit;
  FRowHeight := AValue;
  VisualChange;
end;

procedure TvpGantt.SetGanttBorderWidth(AValue: integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetBorderWidth');
  {$endif}
  if FGanttBorderWidth = AValue then
    Exit;
  FGanttBorderWidth := AValue;
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
      if FvpGanttCalendar.HandleAllocated then
        FvpGanttCalendar.UpdateIntervalDates(FStartDate, FEndDate);
    end;
end;

procedure TvpGantt.SetTaskColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetTaskColor');
  {$endif}
  if FTaskColor=AValue then
    Exit;
  FTaskColor := AValue;
  if FUpdateCount=0 then
    FvpGanttTasks.Invalidate;
end;

procedure TvpGantt.SetCalendarColor(AValue: TColor);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetCalendarColor');
  {$endif}
  if FCalendarColor=AValue then
    Exit;
  FCalendarColor := AValue;
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
  inherited;
  FGanttFocused := true;
  VisualChange;
end;

procedure TvpGantt.RepaintFocus(aFocusRow: integer);
var
  R: TRect;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.UpdateFocus');
  {$endif}
  if FvpGanttTasks.HandleAllocated then
    begin
      R := FvpGanttTasks.CalcRowRect(FFocusRow);
      InvalidateRect(FvpGanttTasks.Handle, @R, true);
    end;
  if FvpGanttCalendar.HandleAllocated then
    begin
      R := FvpGanttCalendar.CalcRowRect(FFocusRow);
      InvalidateRect(FvpGanttCalendar.Handle, @R, true);
    end;
end;

procedure TvpGantt.VisualChange;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.VisualChange INIT');
  {$endif}
  CalcIntervalsHeight;
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

procedure TvpGantt.WMKillFocus(var message: TLMKillFocus);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.WMKillFocus');
  {$endif}
  FGanttFocused := false;
  RepaintFocus(FFocusRow);
end;

function TvpGantt.GetBorderWidth: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetBorderWidth');
  {$endif}
  if FGanttBorderWidth > 0 then
    Result := FGanttBorderWidth
  else
    Result := 0;
end;

function TvpGantt.GetMajorScale: TvpTimeScale;
begin
  if not FvpGanttCalendar.HandleAllocated then
    Result := vptsDay
  else
    Result := FvpGanttCalendar.FMajorScale;
end;

function TvpGantt.GetMajorScaleHeight: integer;
begin
  Result := FMajorScaleHeight;
end;

function TvpGantt.GetMinorScale: TvpTimeScale;
begin
  if not FvpGanttCalendar.HandleAllocated then
    Result := vptsHour
  else
    Result := FvpGanttCalendar.FMinorScale;
end;

function TvpGantt.GetPixelPerMinorScale: integer;
begin
  if FvpGanttCalendar.HandleAllocated then
    Result := FvpGanttCalendar.FPixelePerMinorScale
  else
    Result := C_DEF_PIXEL_PER_MINOR;
end;

function TvpGantt.GetMinorScaleHeight: integer;
begin
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
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetFocusRow');
  {$endif}
  Result := FFocusRow;
end;

procedure TvpGantt.SetFocusRow(AValue: integer);
var
  R: TRect;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SetFocusRow');
  {$endif}
  if (FFocusRow=AValue) OR (AValue>=FIntervals.Count) then
    Exit;
  {TODO -o Vas Сделать прокрутку к месту фокусировки}
  //стерли старый фокус
  RepaintFocus(FFocusRow);
  FFocusRow := AValue;
  //нарисовали новый
  RepaintFocus(FFocusRow);
end;

procedure TvpGantt.SelectNextRow(const ADelta: integer);
var
  aRow: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.SelectNextRow');
  {$endif}
  if FFocusRow>=IntervalCount-1 then
    Exit;
  aRow := FFocusRow + ADelta;
  if (aRow>=0) AND (aRow<IntervalCount-1) then
    begin
      aRow := FFocusRow + ADelta;
      SetFocusRow(aRow);
    end;
end;

function TvpGantt.GetRowPosY(const YPos: integer): integer;
var
  aTH: integer;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.GetRowPosXY');
  {$endif}
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
  InsertControl(FvpGanttTasks);
  FvpGanttTasks.Width := C_DEF_TASKS_WIDTH;
  FvpGanttTasks.Align := alLeft;

  //create splitter
  FSplitter := TSplitter.Create(Self);
  InsertControl(FSplitter);
  //FSplitter.ResizeStyle := rsPattern;
  FSplitter.Left := FvpGanttTasks.Left + FvpGanttTasks.Width + 1;
  FSplitter.Align := alLeft;
  FSplitter.Width := C_DEF_SPLITTER_WIDTH;

  //create vpGanttTasks
  FvpGanttCalendar := TvpGanttCalendar.Create(Self);
  InsertControl(FvpGanttCalendar);
  FvpGanttCalendar.Align := alClient;

  BeginUpdate;

  Options := DefaultGanttOptions;

  Width := C_VPGANTT_WIDTH;
  Height := C_VPGANTT_HEIGHT;

  GanttBorderWidth := C_DEF_BORDER_WIDTH;
  TitleColor := clBtnFace;
  TaskColor := clWindow;
  CalendarColor := clWindow;
  BorderColor := clActiveBorder;
  ScrollBars := ssAutoBoth;
  FRowHighlightColor := GetHighLightColor(clHighlight, C_ROW_HIGHLITE_VALUE);

  RowHeight := C_DEF_ROW_HEIGHT;
  MajorScaleHeight := C_DEF_ROW_HEIGHT;
  MinorScaleHeight := C_DEF_ROW_HEIGHT;

  FTitleFont := TFont.Create;
  FTitleFont.OnChange := @OnTitleFontChanged;
  FTitleFont.Style := [fsBold];
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

  FFocusRow := -1;
  FFocusColor := clBlack;

  TaskTitleCaption := RS_TITLE_TASKS;

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
  SetFocusRow(FIntervals.Count-1);
  UpdateInterval(true);
end;

procedure TvpGantt.InsertInterval(AnIndex: Integer; AnInterval: TvpInterval);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.InsertInterval');
  {$endif}
  FIntervals.Insert(AnIndex, AnInterval);
  SetFocusRow(AnIndex);
  UpdateInterval(true);
end;

procedure TvpGantt.DeleteInterval(AnIndex: Integer);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DeleteInterval');
  {$endif}
  FIntervals.Delete(AnIndex);
  if FFocusRow>AnIndex then
    SetFocusRow(FFocusRow-1);
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
  FIntervals.Remove(AnInterval);
  if FFocusRow>AnIndex then
    SetFocusRow(FFocusRow-1);
  UpdateInterval;
end;

procedure TvpGantt.UpdateInterval(AUpdateDate: boolean = false);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.UpdateInterval');
  {$endif}
  //изменяются даты, пересчитаем только в случае если календарь создан
  if AUpdateDate then
    if FvpGanttCalendar.HandleAllocated then
      FvpGanttCalendar.UpdateIntervalDates(TvpInterval(FIntervals[FFocusRow]).FStartDate,
                                           TvpInterval(FIntervals[FFocusRow]).FFinishDate);
  VisualChange;
end;

procedure TvpGantt.BeginUpdate;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.BeginUpdate');
  {$endif}
  Inc(FUpdateCount);
end;

procedure TvpGantt.EndUpdate(aRefresh: boolean);
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
  if not FvpGanttTasks.HandleAllocated then
    Result := -1;
  Result := FMajorScaleHeight + FMinorScaleHeight;
end;

procedure TvpGantt.DrawThemedCell(ACanvas: TCanvas; aRect: TRect);
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

procedure TvpGantt.DrawFillRect(ACanvas: TCanvas; aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawFillRect');
  {$endif}
  ACanvas.FillRect(aRect);
end;

procedure TvpGantt.DrawHighlightRect(ACanvas: TCanvas; ARect: TRect);
var
  aOldBrush: TBrush;
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawHighlightRect');
  {$endif}
  if (vpgRowHighlight in Options) then
    begin
      AOldBrush := Canvas.Brush;
      if FGanttFocused then
        ACanvas.Brush.Color := FRowHighlightColor
      else
        ACanvas.Brush.Color := cl3DShadow;
      ACanvas.FillRect(ARect);
      ACanvas.Brush := aOldBrush;
    end;
end;

procedure TvpGantt.DrawTitleGrid(ACanvas: TCanvas; aRect: TRect);
begin
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawTitleGrid');
  {$endif}
  if FGanttBorderWidth>0 then
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
          ACanvas.MoveTo(aRect.Left + 1, aRect.Bottom - 1);
          ACanvas.LineTo(aRect.Right - 2, aRect.Bottom - 1);
          ACanvas.LineTo(aRect.Right - 2, aRect.Top + 1);
          //Иначе внешний цвет деаем темнее
          ACanvas.Pen.Color := cl3DDKShadow;
        end;
      ACanvas.MoveTo(aRect.Left, aRect.Bottom);
      ACanvas.LineTo(aRect.Right - 1, aRect.Bottom);
      ACanvas.LineTo(aRect.Right - 1, aRect.Top);
    end;
end;

procedure TvpGantt.DrawTitleCell(ACanvas: TCanvas; aRect: TRect);
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
      DrawFillRect(ACanvas, aRect);
    end;
  //граница
  DrawTitleGrid(ACanvas, aRect);
end;

procedure TvpGantt.DrawTitleText(ACanvas: TCanvas; aRect: TRect; aText: string;
  aAligment: TAlignment = taLeftJustify);
var
  aTextStyle: TTextStyle;
  textRect: TRect;
begin
  {TODO -o Vas Попробовать сделать для первого диапазона не прокручивающуюся надпись  }
  {$ifdef DBGGANTT}
  Form1.Debug('TvpGantt.DrawTitleText');
  {$endif}
  //рисуем названия
  //SetBkMode(ACanvas.Handle, TRANSPARENT);
  //DrawText(ACanvas.Handle, PChar(aText), Length(aText), aRect, DT_CENTER OR DT_SINGLELINE OR DT_VCENTER);
  aTextStyle := FTitleTextStyle;
  aTextStyle.Alignment := aAligment;
  textRect := Rect(aRect.Left + C_TITLE_TEXT_INDENT, aRect.Top, aRect.Right - C_TITLE_TEXT_INDENT, aRect.Bottom);
  ACanvas.TextRect(textRect, textRect.Left, textRect.Top, aText, aTextStyle);
end;

function TvpGantt.FixScrollPosition(const ARange, APage, APos: integer): integer;
var
  maxPos: integer;
begin
  {$ifdef DBGGANTT}
   Form1.Debug('TvpGantt.FixHScrollPosition');
  {$endif}
  //при скролинге позиция считается как Max-Page, поэтому нажо найти сначала
  //коэффициент отношения размера скрытой области к значениям положения ползунка
  //ну, а в конце поправить неточность округления
  maxPos := aRange - MAX(APage - 1, 0);
  {$ifdef DBGGANTT}
  Form1.Debug(Format('maxPos %d', [maxPos]));
  {$endif}
  Result := Ceil(aRange/maxPos*aPos);
  {$ifdef DBGGANTT}
  Form1.Debug(Format('Result %d', [Result]));
  {$endif}
  //if APos = maxPos then
  //  inc(FXScrollPosition, 2*constCellPadding + 2*FvpGantt.GetBorderWidth);
end;

end.

