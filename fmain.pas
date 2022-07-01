unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  WireLoop, Vectors, Math, FileUtil, LazFileUtils, IniFiles, DebugPrint;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonSave: TButton;
    ButtonReload: TButton;
    ComboBoxModel: TComboBox;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
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

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  Basename: String;
begin
  Basename := ExtractFileNameOnly(ExtractFileNameOnly(ComboBoxModel.Text));
  Image1.Picture.SaveToFile(Basename + '.png', 'png');
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
  X, Y: Integer;
  C: Integer;
  V, F: TVector;
  Canv : TCanvas;
  MZ: Integer;
  T0: TDateTime;
begin
  T0 := Now;
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
  Print((Now - T0) * 24 * 60 * 60);
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

