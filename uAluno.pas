unit uAluno;

interface

uses
  uEntity,  uAtribEntity, System.SysUtils;

type
//nome da classe de entidade
  [TableName('ALUNOS')]
  TAluno = class(TGenericEntity)
  private
    //colocar aqui todas as propriedades privadas da classe de acordo com
    //os campos da tabela
    FMatricula:string;
    FNomeAluno:string;
    FEndereco:string;
    FTelefone:string;
    FCPF:string;
    FID_PESSOA: Integer;
    FID_PESSOA_MAIS: Integer;
    FID_PRODUTO: Integer;

  //colocar aqui todos os métodos sets para cada uma das propriedades acima
    procedure setFMatricula(value:string);
    procedure setFNomeAluno(value:string);
    procedure setFEndereco(value:string);
    procedure setFTelefone(value:string);
    procedure setFCPF(value:string);
    procedure setID_PESSOA(const Value: Integer);
    procedure setID_PESSOA_MAIS(const Value: Integer);
    procedure setID_PRODUTO(const Value: Integer);

  public
    //colocar aqui todas as propriedades públicas de acesso aos objetos dessa
    //classe
    [KeyField('MATRICULA')]
    [FieldName('MATRICULA')]
    [PropriedadesCampo('MATRICULA', True, True, '', '', Inner)]
    property Matricula: string read FMatricula write setFMatricula;

    [FieldName('NOME_ALUNO')]
    [PropriedadesCampo('NOME_ALUNO', False, True, '', '', Inner)]
    property Nome:string read FNomeAluno write setFNomeAluno;

    [FieldName('ENDERECO')]
    [PropriedadesCampo('ENDERECO', False, True, '', '', Inner)]
    property Endereco:string read FEndereco write setFEndereco;

    [FieldName('TELEFONE')]
    [PropriedadesCampo('TELEFONE', False, True, '', '', Inner)]
    property Telefone:string read FTelefone write setFTelefone;

    [FieldName('CPF')]
    [PropriedadesCampo('CPF', False, True, '', '', Inner)]
    property CPF:string read FCPF write setFCPF;

    [FieldName('ID_PESSOA')]
    [PropriedadesCampo('ID_PESSOA', False, True, 'PESSOAS', 'ID_PESSOA', Inner)]
    property ID_PESSOA: Integer read FID_PESSOA write setID_PESSOA;

    [PropriedadesCampo('ID_PESSOA_MAIS', False, True, 'PESSOAS', 'ID_PESSOA_MAIS', Inner)]
    property ID_PESSOA_MAIS: Integer read FID_PESSOA_MAIS write setID_PESSOA_MAIS;

    [PropriedadesCampo('ID_PRODUTO', False, True, 'PRODUTOS', 'ID_PRODUTO', Left)]
    property ID_PRODUTO: Integer read FID_PRODUTO write setID_PRODUTO;

    function ToString:string; override;
end;

implementation

procedure TAluno.setFMatricula(value:string);
begin
  FMatricula:= value;
end;

procedure TAluno.setFNomeAluno(value:string);
begin
  FNomeAluno:= value;
end;

procedure TAluno.setFEndereco(value:string);
begin
  FEndereco:= value;
end;

procedure TAluno.setFTelefone(value:string);
begin
  FTelefone:= value;
end;

procedure TAluno.setID_PESSOA(const Value: Integer);
begin
  FID_PESSOA := Value;
end;

procedure TAluno.setID_PESSOA_MAIS(const Value: Integer);
begin
  FID_PESSOA_MAIS := Value;
end;

procedure TAluno.setID_PRODUTO(const Value: Integer);
begin
  FID_PRODUTO := Value;
end;

procedure TAluno.setFCPF(value:string);
begin
  FCPF:= value;
end;

function TAluno.toString;
begin
  Result := ' Matricula: '+ Matricula + ' Nome: ' + Nome + ' Endereco: ' + Endereco +
            ' Fone: ' + Telefone + ' CPF: ' + CPF;
end;

end.
