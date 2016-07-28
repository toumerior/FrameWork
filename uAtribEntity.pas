unit uAtribEntity;

interface

type
  TTipoLigacao = (Inner, Left);

//Atributo para determinar o nome da tabela na entidade a ser usada
type //nome da tabela
  TableName = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(aName: String);
    property Name: string read FName write FName;
  end;

type //determinar se o campo é um campo chave
  KeyField = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(aName: String);
    property Name: string read FName write FName;
  end;

type  //nome do campo na tabela
  FieldName = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(aName: String);
    property Name: string read FName write FName;
  end;

  NomeTabela = class(TCustomAttribute)
  private
    FNome_Tabela: string;
  public
    constructor Create(pNome_Tabela: string);
    property Nome_Tabela: string read FNome_Tabela write FNome_Tabela;
  end;

  NomeCampo = class(TCustomAttribute)
  private
    FNome_Coluna: string;
    FChave_Primaria: Boolean;
  public
    constructor Create(pNome_Coluna: string; pChave_Primaria: Boolean);
    property Nome_Coluna: string read FNome_Coluna write FNome_Coluna;
    property Chave_Primaria: Boolean read FChave_Primaria write FChave_Primaria;
  end;

  ChaveEstrangeira = class(TCustomAttribute)
  private
    FApelido_Tabela_Estrangeira: string;
    FTipo_Ligacao: TTipoLigacao;
    FTabela_Estrangeira: string;
    FColuna_Estrangeira: string;
  public
    constructor Create(pTabela_Estrangeira: string; pApelido_Tab_Estrangeira: string; pColuna_Estrangeira: string; pTipo_Ligacao: TTipoLigacao);
    property Tabela_Estrangeira: string read FTabela_Estrangeira write FTabela_Estrangeira;
    property Apelido_Tabela_Estrangeira: string read FApelido_Tabela_Estrangeira write FApelido_Tabela_Estrangeira;
    property Coluna_Estrangeira: string read FColuna_Estrangeira write FColuna_Estrangeira;
    property Tipo_Ligacao: TTipoLigacao read FTipo_Ligacao write FTipo_Ligacao;
  end;

type
  PropriedadesCampo = class(TCustomAttribute)
  private
    FName: String;
    FChave: Boolean;
    FTabela_Estrangeira: string;
    FSomente_Leitura: Boolean;
    FColuna_Estrangeira: string;
    FTipo_Ligacao: TTipoLigacao;
    FApelido_Tabela_Estrangeira: string;
  public
    constructor Create(aName: String; aChave: Boolean; aSomente_Leitura: Boolean; aTabela_Estrangeira: string; aColuna_Estrangeira: string; aApelido_Tabela_Estrangeira: string; aTipo_Ligacao: TTipoLigacao);
    property Name: string read FName write FName;
    property Chave: Boolean read FChave write FChave;
    property Somente_Leitura: Boolean read FSomente_Leitura write FSomente_Leitura;
    property Tabela_Estrangeira: string read FTabela_Estrangeira write FTabela_Estrangeira;
    property Coluna_Estrangeira: string read FColuna_Estrangeira write FColuna_Estrangeira;
    property Apelido_Tabela_Estrangeira: string read FApelido_Tabela_Estrangeira write FApelido_Tabela_Estrangeira;
    property Tipo_Ligacao: TTipoLigacao read FTipo_Ligacao write FTipo_Ligacao;
  end;

implementation

constructor TableName.Create(aName: String);
begin
  FName := aName
end;

constructor KeyField.Create(aName: String);
begin
  FName := aName;
end;

constructor FieldName.Create(aName: String);
begin
  FName := aName;
end;

{ DescricaoCampo }

constructor PropriedadesCampo.Create(aName: String; aChave: Boolean; aSomente_Leitura: Boolean; aTabela_Estrangeira: string; aColuna_Estrangeira: string; aApelido_Tabela_Estrangeira: string; aTipo_Ligacao: TTipoLigacao);
begin
  FName := aName;
  FChave := aChave;
  FSomente_Leitura := aSomente_Leitura;
  FTabela_Estrangeira := aTabela_Estrangeira;
  FColuna_Estrangeira := aColuna_Estrangeira;
  FApelido_Tabela_Estrangeira := aApelido_Tabela_Estrangeira;
  FTipo_Ligacao := aTipo_Ligacao;
end;

{ ChaveEstrangeira }

constructor ChaveEstrangeira.Create(
  pTabela_Estrangeira: string;
  pApelido_Tab_Estrangeira: string;
  pColuna_Estrangeira: string;
  pTipo_Ligacao: TTipoLigacao
);
begin
  FTabela_Estrangeira := pTabela_Estrangeira;
  FColuna_Estrangeira := pColuna_Estrangeira;
  FApelido_Tabela_Estrangeira := pApelido_Tab_Estrangeira;
  FTipo_Ligacao := pTipo_Ligacao;
end;

{ NomeCampo }

constructor NomeCampo.Create(pNome_Coluna: string; pChave_Primaria: Boolean);
begin
  FNome_Coluna := pNome_Coluna;
  FChave_Primaria := pChave_Primaria;
end;

{ NomeTabela }

constructor NomeTabela.Create(pNome_Tabela: string);
begin
  FNome_Tabela := pNome_Tabela;
end;

end.
