unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Spin, ATShapeLine;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TSpinEdit;
    Line2: TShapeLine;
    Line1: TShapeLine;
    Line3: TShapeLine;
    Shape1: TShape;
    Label2: TLabel;
    Line4: TShapeLine;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    ColorDialog1: TColorDialog;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton3: TRadioButton;
    Label1: TLabel;
    probe: TShapeLine;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure Edit1Change(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure ConfigTLine(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Panel1Click(Sender: TObject);
begin
ColorDialog1.Color:=Probe.LineColor;
if ColorDialog1.Execute then begin
   Probe.LineColor:=ColorDialog1.Color;
   Panel1.Color:=Probe.LineColor;
end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  probe.ArrowFactor:= Edit1.Value;
end;

procedure TForm1.Panel2Click(Sender: TObject);
begin
ColorDialog1.Color:=Probe.ArrowColor;
if ColorDialog1.Execute then begin
   Probe.ArrowColor:=ColorDialog1.Color;
   Panel2.Color:=Probe.ArrowColor;
end;
end;

procedure TForm1.ConfigTLine(Sender: TObject);
begin
Probe.Arrow1:=CheckBox1.Checked;
Probe.Arrow2:=CheckBox2.Checked;
if RadioButton1.Checked then
   Probe.Direction:=drLeftRight
else
if RadioButton2.Checked then
   Probe.Direction:=drUpDown
else
if RadioButton3.Checked then
   Probe.Direction:=drTopLeftBottomRight
else
   Probe.Direction:=drTopRightBottomLeft
end;


end.
