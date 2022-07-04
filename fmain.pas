unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  WireLoop, Vectors, Math, FileUtil, LazFileUtils, IniFiles, DebugPrint,
  UTF8Process, gdeque, syncobjs;

type
  TLineResult = class
    FY: Integer;
    FColors: array of TColor;
  end;

  TResultQueue = specialize TDeque<TLineResult>;

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    FYStart: Integer;
    FYEnd: Integer;
  public
    constructor Create(YStart, YEnd: Integer);
    procedure Execute; override;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    ButtonSave: TButton;
    ButtonReload: TButton;
    ComboBoxModel: TComboBox;
    Image1: TImage;
    Timer1: TTimer;
    procedure ButtonReloadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ComboBoxModelChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearDrawing;
    procedure DrawWire;
    procedure DrawField;
    procedure FormResize(Sender: TObject);
    procedure PostResult(R: TLineResult);
    function PopResult: TLineResult;
    procedure Timer1Timer(Sender: TObject);
  private
    FIni: TIniFile;
    FLoop: TWireLoop;
    FWantStop: Boolean;
    FResultQueue : TResultQueue;
    FResultLock: TCriticalSection;
    FThreadCount: Integer;
    FMustRedraw: Boolean;
    procedure FillCombo;
    procedure Redraw;
    procedure StopAndWaitThreads;
   public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TWorkerThread }

constructor TWorkerThread.Create(YStart, YEnd: Integer);
begin
  FYStart := YStart;
  FYEnd := YEnd;
  Inc(FormMain.FThreadCount);
  inherited Create(False);
end;

procedure TWorkerThread.Execute;
var
  X, Y, W: Integer;
  V, F: TVector;
  MagZ: Integer;
  LR: TLineResult;
  C, C1: Byte;
begin
  FreeOnTerminate := True;
  W := FormMain.Image1.ClientRect.Width;
  for Y := FYStart to FYEnd do begin
    LR := TLineResult.Create;
    LR.FY := Y;
    SetLength(LR.FColors, W);
    for X := 0 to W - 1 do begin
      V := FormMain.FLoop.PlotPlane.ScreenToVector(X, Y);
      V.Z += FormMain.FLoop.SensorHeight;

      F := FormMain.FLoop.CalcFieldAt(V);

      // compress the range using square root
      MagZ := Round(Sqrt(abs(F.Z)));
      if MagZ > 255 then MagZ := 255;

      C := 255 - MagZ;
      if F.Z > 0 then begin
        // inside of loop
        C1 := 255 - MagZ div 2;
        LR.FColors[X] := RGBToColor(C, C1, C);
      end
      else begin
        // outside of loop
        LR.FColors[X] := RGBToColor(255, C, C);
      end;
    end;
    FormMain.PostResult(LR);

    if FormMain.FWantStop or Terminated then
      break;
  end;
  InterlockedDecrement(FormMain.FThreadCount);
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  S: String;
begin
  FResultLock := TCriticalSection.Create;
  FResultQueue := TResultQueue.Create;
  S := ExtractFileNameWithoutExt(Application.ExeName);
  FIni := TIniFile.Create(S + '.ini');
  Top := FIni.ReadInteger('UI', 'WinTop', Top);
  Left := FIni.ReadInteger('UI', 'WinLeft', Left);
  Width := FIni.ReadInteger('UI', 'WinWidth', Width);
  Height := FIni.ReadInteger('UI', 'WinHeight', Height);
  FillCombo;
  FLoop := TWireLoop.Create;
  FillCombo;
  ComboBoxModel.Text := FIni.ReadString('model', 'name', '');
  ComboBoxModelChange(nil);
end;

procedure TFormMain.ButtonReloadClick(Sender: TObject);
begin
  ComboBoxModelChange(nil);
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  Basename: String;
begin
  Basename := ExtractFileNameOnly(ExtractFileNameOnly(ComboBoxModel.Text));
  Image1.Picture.SaveToFile(Basename + '.png', 'png');
end;

procedure TFormMain.ComboBoxModelChange(Sender: TObject);
begin
  StopAndWaitThreads;
  FLoop.LoadLoop(ComboBoxModel.Text);
  FIni.WriteString('model', 'name', ComboBoxModel.Text);
  FMustRedraw := True;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FWantStop := True;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FIni.WriteInteger('UI', 'WinTop', Top);
  FIni.WriteInteger('UI', 'WinLeft', Left);
  FIni.WriteInteger('UI', 'WinWidth', Width);
  FIni.WriteInteger('UI', 'WinHeight', Height);
  FIni.UpdateFile;
  FreeAndNil(FIni);
  FreeAndNil(FLoop);
  FreeAndNil(FResultQueue);
  FreeAndNil(FResultLock);
end;

procedure TFormMain.ClearDrawing;
begin
  FLoop.PlotPlane.SetScreenRect(Image1.ClientRect);
  FLoop.Resoluton := 0.5;
  with image1.Picture.Bitmap do begin
    SetSize(Image1.Width, Image1.Height);
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(0, 0, Image1.Width, Image1.Height);
  end;
end;

procedure TFormMain.DrawWire;
var
  P, V: TVector;
  P0, P1: TPoint;
  Plane: TPlotPlane;
  Canv: TCanvas;

  procedure DrawScale(X, Y, Size: Integer; Txt: String);
  var
    X1, Y1: Integer;
  begin
    X1 := X + Size * Round(Plane.Scale);
    Y1 := Y;
    Canv.Pen.Color := clBlack;
    Canv.Font.Size := 10;
    Canv.Brush.Style := bsClear;
    Canv.Line(X, Y, X1, Y1);
    Canv.Line(X, Y-5, X, Y+5);
    Canv.Line(X1, Y1-5, X1, Y1+5);
    Canv.TextOut(X1 + 10, Y1 - 10, Txt);
  end;

begin
  Plane := FLoop.PlotPlane;
  Canv := image1.Picture.Bitmap.Canvas;
  Canv.Pen.Color := clGreen;
  Canv.Pen.Width := 1;

  P := Vector(0, 0, 0);
  P0 := Plane.VectorToScreen(P);
  for V in FLoop.Segments do begin
    P.Add(V);
    P1 := Plane.VectorToScreen(P);
    Canv.Line(P0, P1);
    P0 := P1;
  end;

  p0.X := Image1.ClientRect.Left;
  p0.Y := Image1.ClientRect.Bottom;
  DrawScale(P0.X + 10, P0.Y - 27, 10, '10 cm');
  DrawScale(P0.X + 10, P0.Y - 15, 100, '1 m');

  P0 := Image1.ClientRect.TopLeft;
  Canv.TextOut(P0.X + 10, P0.Y + 10, Format('Model: %s',
    [ExtractFileNameOnly(ExtractFileNameOnly(ComboBoxModel.Text))]));
  Canv.TextOut(P0.X + 10, P0.Y + 22, Format('Height above wire: %g cm',
    [FLoop.SensorHeight]));
end;

procedure TFormMain.DrawField;
var
  NumThreads: Integer;
  Y, Y2, X, H, I: Integer;
  C, C1: Byte;
  LR: TLineResult;
  Canv: TCanvas;
  WireHiddenCounter: Integer;
  Bitmap: TBitmap;
  SL: PUInt32;
  Raw: PUInt32;

  Mag: Integer;
  Dir: Boolean;

  T0: TDateTime;
begin
  T0 := Now;
  FWantStop := False;
  DrawWire;
  WireHiddenCounter := 0;
  NumThreads := GetSystemThreadCount;
  Y := 0;
  H := Image1.ClientRect.Height div NumThreads;
  for I := 0 to NumThreads - 1 do begin
    Y2 := Y + H;
    if Y2 > Image1.ClientRect.Height then
      Y2 := Image1.ClientRect.Height;
    TWorkerThread.Create(Y, Y2);
    Y := Y2 + 1;
  end;

  Bitmap := Image1.Picture.Bitmap;
  Canv := Bitmap.Canvas;

  repeat
    LR := PopResult;
    if Assigned(LR) then begin
      Y := LR.FY;
      for X := 0 to Length(LR.FColors) - 1 do begin
        Canv.Pixels[X,Y] := LR.FColors[X];
      end;
      LR.Free;
      Inc(WireHiddenCounter);
      if WireHiddenCounter > 64 then begin
        DrawWire;
        Application.ProcessMessages;
        WireHiddenCounter := 0;
      end;
    end
    else begin
      if WireHiddenCounter > 0 then begin
        DrawWire;
        WireHiddenCounter := 0;
      end;
      Application.ProcessMessages;
      Sleep(1);
    end;
  until (FThreadCount = 0) and FResultQueue.IsEmpty;
  if WireHiddenCounter > 0 then
    DrawWire;
  Print('time: %f s', [(Now - T0) * 24 * 60 * 60]);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  StopAndWaitThreads;
  FMustRedraw := True;
end;

procedure TFormMain.PostResult(R: TLineResult);
begin
  FResultLock.Acquire;
  FResultQueue.PushBack(R);
  FResultLock.Release;
end;

function TFormMain.PopResult: TLineResult;
begin
  if FResultQueue.IsEmpty then
    Result := nil
  else begin
    FResultLock.Acquire;
    Result := FResultQueue.Front;
    FResultQueue.PopFront;
    FResultLock.Release;
  end;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  if FMustRedraw then begin
    FMustRedraw := False;
    Redraw;
  end;
end;

procedure TFormMain.FillCombo;
var
  SL: TStringList;
  S, T: String;
begin
  T := ComboBoxModel.Text;
  ComboBoxModel.Items.Clear;
  SL := TStringList.Create;
  FindAllFiles(SL, GetCurrentDir, '*.model.txt', False);
  for S in SL do begin
    ComboBoxModel.Items.Append(ExtractFileName(S));
  end;
  SL.Free;
  ComboBoxModel.Text := T;
end;

procedure TFormMain.Redraw;
begin
  StopAndWaitThreads;
  ClearDrawing;
  DrawField;
end;

procedure TFormMain.StopAndWaitThreads;
begin
  FWantStop := True;
  while FThreadCount > 0 do begin
    Sleep(1);
  end;
  FResultQueue.Clear;
end;

end.

