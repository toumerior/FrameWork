unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uAluno, uGenericDAO, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    btn1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  aluno: TAluno;
begin
  aluno := TAluno.Create;
  aluno.Nome := 'BRUNO';
  aluno.Matricula := '0028';
  aluno.Endereco := 'Lugar nenhum';
  aluno.Telefone := '2321';
  aluno.CPF := '04923311118';

  if TGenericDAO.Update(aluno) then
    ShowMessage('Funfou');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  aluno: TAluno;
begin
  aluno := TAluno.Create;
  aluno.Nome := 'BRUNO';
  aluno.Matricula := '0028';
  aluno.Endereco := 'Lugar nenhum';
  aluno.Telefone := '2321';
  aluno.CPF := '04923311118';

  if TGenericDAO.Insert(aluno) then
    ShowMessage('Funfou');
end;

end.
