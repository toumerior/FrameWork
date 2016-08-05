program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uGenericDAO in 'uGenericDAO.pas',
  uAtribEntity in 'uAtribEntity.pas',
  uEntity in 'uEntity.pas',
  uTiposPrimitivos in 'uTiposPrimitivos.pas',
  Foo in 'Foo.pas',
  ClassesTeste in 'ClassesTeste.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
