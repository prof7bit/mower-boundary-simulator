unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  WireLoop, Vectors, Math, FileUtil, LazFileUtils, IniFiles, DebugPrint;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonReload: TButton;
    ComboBoxModel: TComboBox;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure ButtonReloadClick(Sender: TObject);
    procedure ComboBoxModelChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearDrawing;
    procedure DrawWire;
    procedure DrawField;
    procedure FormResize(Sender: TObject);
  private
    FIni: TIniFile;
    FLoop: TWireLoop;
    FWantClose: Boolean;
    procedure FillCombo;
    procedure AsyncRedraw(Data: PtrInt);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  S: String;
begin
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

procedure TFormMain.Button1Click(Sender: TObject);
begin
  DrawField;
end;

procedure TFormMain.ButtonReloadClick(Sender: TObject);
begin
  ComboBoxModelChange(nil);
end;

procedure TFormMain.ComboBoxModelChange(Sender: TObject);
begin
  FLoop.LoadLoop(ComboBoxModel.Text);
  FIni.WriteString('model', 'name', ComboBoxModel.Text);
  Application.QueueAsyncCall(@AsyncRedraw, 0);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FWantClose := True;
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
  begin
    P0.X := X;
    P0.Y := Y;
    P1.X := P0.X + Size * Round(Plane.Scale);
    P1.Y := P0.Y;
    Canv.Pen.Color := clBlack;
    Canv.Font.Size := 10;
    Canv.Brush.Style := bsClear;
    Canv.Line(P0, P1);
    Canv.Line(P0.X, P0.Y-5, P0.X, P0.Y+5);
    Canv.Line(P1.X, P1.Y-5, P1.X, P1.Y+5);
    Canv.TextOut(P1.X + 10, P1.Y - 10, Txt);
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

  P := VectorXY(Plane.X0, Plane.Y0);
  P0 := Plane.VectorToScreen(P);
  DrawScale(P0.X, P0.Y, 10, '10 cm');
  DrawScale(P0.X, P0.Y + 12, 100, '1 m');

  P := VectorXY(Plane.X0, Plane.YMax + 15);
  P0 := Plane.VectorToScreen(P);
  Canv.TextOut(P0.X, P0.Y, Format('Height above wire: %g cm',
    [FLoop.SensorHeight]));
  Canv.TextOut(P0.X, P0.Y - 15, Format('Model: %s',
    [ExtractFileNameOnly(ExtractFileNameOnly(ComboBoxModel.Text))]));
end;

procedure TFormMain.DrawField;
var
  X, Y: Integer;
  C: Integer;
  V, F: TVector;
  Canv : TCanvas;
  MZ: Integer;
begin
  FWantClose := False;
  FLoop.Resoluton := 1;
  Canv := Image1.Picture.Bitmap.Canvas;
  for X := Image1.ClientRect.Left to Image1.ClientRect.Right do begin
    for Y := Image1.ClientRect.Top to Image1.ClientRect.Bottom do begin
      V := FLoop.PlotPlane.ScreenToVector(X, Y);

      V.Z += FLoop.SensorHeight;
      F := FLoop.CalcFieldAt(V);

      MZ := Round(F.Z);
      if MZ > 255 then
        MZ := 255;
      if MZ < -255 then
        MZ := -255;

      if MZ > 0 then begin
        // inside of loop
        C := 255 - MZ;
        Canv.Pixels[X,Y] := RGBToColor(C, 255, C);;
      end
      else begin
        // outside of loop
        C := 255 + MZ;
        Canv.Pixels[X,Y] := RGBToColor(255, C, C);;
      end;

    end;
    DrawWire;
    Application.ProcessMessages;
    if FWantClose then
      Break;
  end;
  DrawWire;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  FWantClose := True;
  Application.QueueAsyncCall(@AsyncRedraw, 0);
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

procedure TFormMain.AsyncRedraw(Data: PtrInt);
begin
  ClearDrawing;
  DrawField;
end;

end.

