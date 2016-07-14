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

type
  PropriedadesCampo = class(TCustomAttribute)
  private
    FName: String;
    FChave: Boolean;
    FTabela_Estrangeira: string;
    FSomente_Leitura: Boolean;
    FColuna_Estrangeira: string;
    FTipo_Ligacao: TTipoLigacao;
  public
    constructor Create(aName: String; aChave: Boolean; aSomente_Leitura: Boolean; aTabela_Estrangeira: string; aColuna_Estrangeira: string; aTipo_Ligacao: TTipoLigacao);
    property Name: string read FName write FName;
    property Chave: Boolean read FChave write FChave;
    property Somente_Leitura: Boolean read FSomente_Leitura write FSomente_Leitura;
    property Tabela_Estrangeira: string read FTabela_Estrangeira write FTabela_Estrangeira;
    property Coluna_Estrangeira: string read FColuna_Estrangeira write FColuna_Estrangeira;
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

constructor PropriedadesCampo.Create(aName: String; aChave: Boolean; aSomente_Leitura: Boolean; aTabela_Estrangeira: string; aColuna_Estrangeira: string; aTipo_Ligacao: TTipoLigacao);
begin
  FName := aName;
  FChave := aChave;
  FSomente_Leitura := aSomente_Leitura;
  FTabela_Estrangeira := aTabela_Estrangeira;
  FColuna_Estrangeira := aColuna_Estrangeira;
  FTipo_Ligacao := aTipo_Ligacao;
end;

end.
