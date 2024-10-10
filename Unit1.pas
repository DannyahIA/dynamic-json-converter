unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  System.Rtti,
  System.JSON,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  Vcl.ComCtrls,
  DateUtils,

  // GUIA DTO
  GuiaMonitoramentoDto;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    tabParseJSON: TTabSheet;
    TabSheet2: TTabSheet;
    pnlParseJSON: TPanel;
    mmEntrada: TMemo;
    mmSaida: TMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    btmParse: TButton;
    lblSaida: TLabel;
    Splitter3: TSplitter;
    lblEntrada: TLabel;
    pnlLabelParse: TPanel;
    Panel3: TPanel;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    Splitter6: TSplitter;
    mmEntrada2: TMemo;
    mmSaida2: TMemo;
    btmConvert: TButton;
    pnlLabelObj: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure btObjToJsonClick(Sender: TObject);
    procedure btmParseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  // CLASSES EXEMPLO
type
  TProduto = class(TObject)
  private
    FNome: string;
    FPreco: double;
    FProdutos: TArray<TProduto>;
  public
    property Nome: string read FNome write FNome;
    property Preco: double read FPreco write FPreco;
    property Produtos: TArray<TProduto> read FProdutos write FProdutos;
  end;

  TLoja = class
  private
    FNome: string;
    FProdutos: TArray<TProduto>;
  public
    property Nome: string read FNome write FNome;
    property Produtos: TArray<TProduto> read FProdutos write FProdutos;
  end;

  // CLASSES EXEMPLO 2
type
  TCarro = class
  private
    FNome: string;
    FModelo: string;
  public
    property Nome: string read FNome write FNome;
    property Modelo: string read FModelo write FModelo;
  end;

  TMarca = class
  private
    FNome: string;
    FCarros: TArray<TCarro>;
  public
    property Nome: string read FNome write FNome;
    property Carros: TArray<TCarro> read FCarros write FCarros;
  end;

  // OTHER
  TJsonDynamicConverter = class
    procedure JSONToObject(AObject: TObject; AJSON: TJSONObject);
    procedure ObjectToJSON(AObject: TObject; var AJSON: TJSONObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// JSON PARA OBJETO
procedure TForm1.btmParseClick(Sender: TObject);
var
  GuiaMonitoramento: TGuiaMonitoramento;
  JSONObj: TJSONObject;
begin
  // Parse da string para um JsonObject
  JSONObj := TJSONObject.ParseJSONValue(mmEntrada.Text) as TJSONObject;
  try
    GuiaMonitoramento := TGuiaMonitoramento.Create;
    try
      TJsonDynamicConverter.Create.JSONToObject(GuiaMonitoramento, JSONObj); //Popula o objeto com os dados do json
      mmSaida.Clear;

      mmSaida.Lines.Add('Guia: ');
      mmSaida.Lines.Add('GuiaId: ' + GuiaMonitoramento.GuiaId);
      mmSaida.Lines.Add('RegistroAns: ' + GuiaMonitoramento.RegistroAns);
      mmSaida.Lines.Add('TipoRegistroId: ' + GuiaMonitoramento.TipoRegistroId);
      mmSaida.Lines.Add('VersaoTissPrestador: ' +
        GuiaMonitoramento.VersaoTissPrestador);
      mmSaida.Lines.Add('FormaEnvioId: ' + GuiaMonitoramento.FormaEnvioId);
      mmSaida.Lines.Add('PrestadorCnesId: ' +
        GuiaMonitoramento.PrestadorCnesId);
      mmSaida.Lines.Add('PrestadorIndicadorIdentificacaoId: ' +
        GuiaMonitoramento.PrestadorIndicadorIdentificacaoId);
      mmSaida.Lines.Add('PrestadorCnpjCpf: ' +
        GuiaMonitoramento.PrestadorCnpjCpf);
      mmSaida.Lines.Add('PrestadorMunicipioId: ' +
        GuiaMonitoramento.PrestadorMunicipioId);
      mmSaida.Lines.Add('BeneficiarioCns: ' +
        GuiaMonitoramento.BeneficiarioCns);
      mmSaida.Lines.Add('BeneficiarioCpf: ' +
        GuiaMonitoramento.BeneficiarioCpf);
      mmSaida.Lines.Add('BeneficiarioSexoId: ' +
        GuiaMonitoramento.BeneficiarioSexoId);
      mmSaida.Lines.Add('BeneficiarioDataNascimento: ' +
        DateToStr(GuiaMonitoramento.BeneficiarioDataNascimento));
      mmSaida.Lines.Add('BeneficiarioMunicipioId: ' +
        GuiaMonitoramento.BeneficiarioMunicipioId);
      mmSaida.Lines.Add('BeneficiarioNumeroRegistroPlano: ' +
        GuiaMonitoramento.BeneficiarioNumeroRegistroPlano);
      mmSaida.Lines.Add('TipoGuiaId: ' + GuiaMonitoramento.TipoGuiaId);
      mmSaida.Lines.Add('OrigemGuiaId: ' + GuiaMonitoramento.OrigemGuiaId);
      mmSaida.Lines.Add('Competencia: ' + GuiaMonitoramento.Competencia);
      mmSaida.Lines.Add('NumeroGuiaPrestador: ' +
        GuiaMonitoramento.NumeroGuiaPrestador);
      mmSaida.Lines.Add('NumeroGuiaOperadora: ' +
        GuiaMonitoramento.NumeroGuiaOperadora);
      mmSaida.Lines.Add('IdentificadorReembolso: ' +
        GuiaMonitoramento.IdentificadorReembolso);
      mmSaida.Lines.Add('IdentificacaoValorPreestabelecido: ' +
        GuiaMonitoramento.IdentificacaoValorPreestabelecido);
      mmSaida.Lines.Add('GuiaSolicitacaoInternacao: ' +
        GuiaMonitoramento.GuiaSolicitacaoInternacao);
      mmSaida.Lines.Add('DataSolicitacao: ' +
        DateToStr(GuiaMonitoramento.DataSolicitacao));
      mmSaida.Lines.Add('NumeroGuiaSpsadtPrincipal: ' +
        GuiaMonitoramento.NumeroGuiaSpsadtPrincipal);
      mmSaida.Lines.Add('DataAutorizacao: ' +
        DateToStr(GuiaMonitoramento.DataAutorizacao));
      mmSaida.Lines.Add('DataRealizacao: ' +
        DateToStr(GuiaMonitoramento.DataRealizacao));
      mmSaida.Lines.Add('DataProtocoloCobranca: ' +
        DateToStr(GuiaMonitoramento.DataProtocoloCobranca));
      mmSaida.Lines.Add('DataPagamento: ' +
        DateToStr(GuiaMonitoramento.DataPagamento));
      mmSaida.Lines.Add('DataProcessamentoGuia: ' +
        DateToStr(GuiaMonitoramento.DataProcessamentoGuia));
      mmSaida.Lines.Add('DataFimPeriodo: ' +
        DateToStr(GuiaMonitoramento.DataFimPeriodo));
      mmSaida.Lines.Add('DataInicialFaturamento: ' +
        DateToStr(GuiaMonitoramento.DataInicialFaturamento));
      mmSaida.Lines.Add('ValorTotalInformado: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalInformado));
      mmSaida.Lines.Add('ValorProcessado: ' +
        FloatToStr(GuiaMonitoramento.ValorProcessado));
      mmSaida.Lines.Add('ValorTotalPagoProcedimento: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalPagoProcedimento));
      mmSaida.Lines.Add('ValorTotalDiaria: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalDiaria));
      mmSaida.Lines.Add('ValorTotalTaxa: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalTaxa));
      mmSaida.Lines.Add('ValorTotalMaterial: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalMaterial));
      mmSaida.Lines.Add('ValorTotalOpme: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalOpme));
      mmSaida.Lines.Add('ValorTotalMedicamento: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalMedicamento));
      mmSaida.Lines.Add('ValorGlosaGuia: ' +
        FloatToStr(GuiaMonitoramento.ValorGlosaGuia));
      mmSaida.Lines.Add('ValorPagoGuia: ' +
        FloatToStr(GuiaMonitoramento.ValorPagoGuia));
      mmSaida.Lines.Add('ValorPagoFornecedor: ' +
        FloatToStr(GuiaMonitoramento.ValorPagoFornecedor));
      mmSaida.Lines.Add('ValorTotalPagoTabelaPropria: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalPagoTabelaPropria));
      mmSaida.Lines.Add('ValorTotalPagoCoparticipacao: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalPagoCoparticipacao));
      mmSaida.Lines.Add('IndicacaoRecemNato: ' +
        GuiaMonitoramento.IndicacaoRecemNato);
      mmSaida.Lines.Add('IndicacaoAcidente: ' +
        GuiaMonitoramento.IndicacaoAcidente);
      mmSaida.Lines.Add('CaraterAtendimento: ' +
        GuiaMonitoramento.CaraterAtendimento);
      mmSaida.Lines.Add('TipoInternacao: ' + GuiaMonitoramento.TipoInternacao);
      mmSaida.Lines.Add('RegimeInternacao: ' +
        GuiaMonitoramento.RegimeInternacao);
      mmSaida.Lines.Add('SaudeOcupacional: ' +
        GuiaMonitoramento.SaudeOcupacional);
      mmSaida.Lines.Add('TipoFaturamento: ' +
        GuiaMonitoramento.TipoFaturamento);
      mmSaida.Lines.Add('DiariaAcompanhante: ' +
        IntToStr(GuiaMonitoramento.DiariaAcompanhante));
      mmSaida.Lines.Add('DiariaUti: ' + IntToStr(GuiaMonitoramento.DiariaUti));
      mmSaida.Lines.Add('MotivoSaidaId: ' + GuiaMonitoramento.MotivoSaidaId);
      mmSaida.Lines.Add('CboExecutante: ' + GuiaMonitoramento.CboExecutante);
      mmSaida.Lines.Add('TipoConsulta: ' + GuiaMonitoramento.TipoConsulta);
      mmSaida.Lines.Add('RegistroAnsOperadoraIntermediaria: ' +
        GuiaMonitoramento.RegistroAnsOperadoraIntermediaria);
      mmSaida.Lines.Add('TipoAtendimentoIntermediarioId: ' +
        GuiaMonitoramento.TipoAtendimentoIntermediarioId);
      mmSaida.Lines.Add('TipoAtendimentoId: ' +
        GuiaMonitoramento.TipoAtendimentoId);
      mmSaida.Lines.Add('RegimeAtendimentoId: ' +
        GuiaMonitoramento.RegimeAtendimentoId);
      mmSaida.Lines.Add('FormasRemuneracaoId: ' +
        GuiaMonitoramento.FormasRemuneracaoId);
      mmSaida.Lines.Add('ValorRemuneracaoId: ' +
        FloatToStr(GuiaMonitoramento.ValorRemuneracaoId));
      mmSaida.Lines.Add('StatusAnsId: ' + GuiaMonitoramento.StatusAnsId);
      mmSaida.Lines.Add('GuiaPendente: ' + GuiaMonitoramento.GuiaPendente);
      mmSaida.Lines.Add('StatusEnviaAns: ' + GuiaMonitoramento.StatusEnviaAns);
      mmSaida.Lines.Add('ChaveAcesso: ' + GuiaMonitoramento.ChaveAcesso);
      mmSaida.Lines.Add(SLineBreak);

      for var FormaRemuneracao in GuiaMonitoramento.FormasRemuneracao do
      begin
        mmSaida.Lines.Add('Forma Remuneração: ');

        mmSaida.Lines.Add(' Guia Forma Remuneração Id: ' +
          FormaRemuneracao.GuiaFormaRemuneracaoId);
        mmSaida.Lines.Add(' Guia Id: ' + FormaRemuneracao.GuiaId);
        mmSaida.Lines.Add(' Forma Remuneração Id: ' +
          FormaRemuneracao.FormaRemuneracaoId);
        mmSaida.Lines.Add(' Valor Remuneração: ' +
          FloatToStr(FormaRemuneracao.ValorRemuneracao));

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var DiagnosticoCid10 in GuiaMonitoramento.DiagnosticosCid10 do
      begin
        mmSaida.Lines.Add('Diagnóstico CID-10: ');

        mmSaida.Lines.Add(' CID-10 Id: ' + DiagnosticoCid10.Cid10Id);
        mmSaida.Lines.Add(' Sequência: ' +
          IntToStr(DiagnosticoCid10.Sequencia));

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var DeclaracaoNascido in GuiaMonitoramento.DeclaracoesNascido do
      begin
        mmSaida.Lines.Add('Declaração Nascido: ');

        mmSaida.Lines.Add(' Declaração Nascido Id: ' +
          DeclaracaoNascido.DeclaracaoNascidoId);
        mmSaida.Lines.Add(' Guia Id: ' + DeclaracaoNascido.GuiaId);
        mmSaida.Lines.Add(' Sequência: ' +
          IntToStr(DeclaracaoNascido.Sequencia));
        mmSaida.Lines.Add(' Numero Declaração: ' +
          DeclaracaoNascido.NumeroDeclaracao);

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var DeclaracaoObito in GuiaMonitoramento.DeclaracoesObito do
      begin
        mmSaida.Lines.Add('Declaração Óbito: ');

        mmSaida.Lines.Add(' Declaração Óbito Id: ' +
          DeclaracaoObito.DeclaracaoObitoId);
        mmSaida.Lines.Add(' Guia Id: ' + DeclaracaoObito.GuiaId);
        mmSaida.Lines.Add(' Sequência: ' + IntToStr(DeclaracaoObito.Sequencia));
        mmSaida.Lines.Add(' Numero Declaração: ' +
          DeclaracaoObito.NumeroDeclaracao);

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var Procedimento in GuiaMonitoramento.Procedimentos do
      begin
        mmSaida.Lines.Add('Procedimento: ');

        mmSaida.Lines.Add(' Procedimento Id: ' + Procedimento.ProcedimentoId);
        mmSaida.Lines.Add(' Guia Id: ' + Procedimento.GuiaId);
        mmSaida.Lines.Add(' Sequência: ' + IntToStr(Procedimento.Sequencia));
        mmSaida.Lines.Add(' Código Tabela Monitor Id: ' +
          Procedimento.CodigoTabelaMonitorId);
        mmSaida.Lines.Add(' Grupo Procedimento Id: ' +
          Procedimento.GrupoProcedimentoId);
        mmSaida.Lines.Add(' Código Procedimento Id: ' +
          Procedimento.CodigoProcedimentoId);
        mmSaida.Lines.Add(' Dente Id: ' + Procedimento.DenteId);
        mmSaida.Lines.Add(' Região Boca Id: ' + Procedimento.RegiaoBocaId);
        mmSaida.Lines.Add(' Dente Face Id: ' + Procedimento.DenteFaceId);
        mmSaida.Lines.Add(' Quantidade Informada: ' +
          FloatToStr(Procedimento.QuantidadeInformada));
        mmSaida.Lines.Add(' Valor Informado: ' +
          FloatToStr(Procedimento.ValorInformado));
        mmSaida.Lines.Add(' Quantidade Paga: ' +
          FloatToStr(Procedimento.QuantidadePaga));
        mmSaida.Lines.Add(' Unidade Medida Id: ' +
          Procedimento.UnidadeMedidaId);
        mmSaida.Lines.Add(' Valor Pago Procedimento: ' +
          FloatToStr(Procedimento.ValorPagoProcedimento));
        mmSaida.Lines.Add(' Valor Pago Fornecedor: ' +
          FloatToStr(Procedimento.ValorPagoFornecedor));
        mmSaida.Lines.Add(' Cnpj Fornecedor: ' + Procedimento.CnpjFornecedor);
        mmSaida.Lines.Add(' Valor Coparticipação: ' +
          FloatToStr(Procedimento.ValorCoparticipacao));

        mmSaida.Lines.Add(SLineBreak);

        for var ItemPacote in Procedimento.ItemsPacote do
        begin
          mmSaida.Lines.Add('Item Pacote: ');

          mmSaida.Lines.Add(' Item Pacote Id: ' + ItemPacote.ItemPacoteId);
          mmSaida.Lines.Add(' Guia Procedimento Id: ' +
            ItemPacote.GuiaProcedimentoId);
          mmSaida.Lines.Add(' Código Tabela Pacote Id: ' +
            ItemPacote.CodigoTabelaPacoteId);
          mmSaida.Lines.Add(' Código Procedimento: ' +
            ItemPacote.CodigoProcedimento);
          mmSaida.Lines.Add(' Quantidade: ' +
            FloatToStr(ItemPacote.Quantidade));

          mmSaida.Lines.Add(SLineBreak);
        end;
      end;

    finally
      GuiaMonitoramento.Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

// OBJETO PARA JSON
procedure TForm1.btObjToJsonClick(Sender: TObject);
var
  // Variaveis para popular objeto com json
  Loja: TLoja;
  JSONEntrada: TJSONObject;

  // Variaveis de conversão de objeto para json
  JSON: TJSONObject;
  SomeClass: TJsonDynamicConverter;
begin
  JSONEntrada := TJSONObject.ParseJSONValue(mmEntrada2.Text) as TJSONObject;
  try
    Loja := TLoja.Create;
    try
      SomeClass := TJsonDynamicConverter.Create;
      SomeClass.JSONToObject(Loja, JSONEntrada);

      JSON := TJSONObject.Create;
      SomeClass.ObjectToJSON(Loja, JSON);
      try
        mmSaida2.Clear;
        mmSaida2.Lines.Add(TJSONValue.Create.ParseJSONValue(JSON.ToString).Format());
      finally
        JSON.Free;
      end;
    finally
      Loja.Free;
    end;
  finally
    JSONEntrada.Free;
  end;
end;

{ TJsonDynamicConverter }

procedure TJsonDynamicConverter.JSONToObject(AObject: TObject; AJSON: TJSONObject);
var
  Context: TRttiContext; // Contexto RTTI para acessar as propriedades do objeto
  RttiType: TRttiType; // Representa o tipo do objeto usando RTTI
  Prop: TRttiProperty; // Propriedade RTTI para cada campo no objeto
  JsonPair: TJSONPair; // Representa um par de chave-valor no JSON
  JsonArray: TJSONArray; // Representa um array no JSON
  ElemClass: TClass; // Classe dos elementos no array dinâmico
  I: integer; // Iterador de loop
  SubObject: TObject; // Objeto auxiliar para elementos de arrays
  ArrayValue: TValue; // Valor do array final para ser atribuído à propriedade
  TValueArray: TArray<TValue>;
  // Array de valores para os elementos do array dinâmico
begin
  Context := TRttiContext.Create; // Inicializa o contexto RTTI
  try
    RttiType := Context.GetType(AObject.ClassType);
    // Obtém o tipo RTTI do objeto

    // Itera sobre cada par chave-valor do JSON
    for JsonPair in AJSON do
    begin
      // Tenta obter a propriedade do objeto correspondente à chave do JSON
      Prop := RttiType.GetProperty(JsonPair.JsonString.Value);

      // Verifica se a propriedade existe no objeto
      if Assigned(Prop) then
      begin
        // Verifica se a propriedade é um array dinâmico e se o valor no JSON é um array
        if (Prop.PropertyType.TypeKind = tkDynArray) and
          (JsonPair.JsonValue is TJSONArray) then
        begin
          JsonArray := TJSONArray(JsonPair.JsonValue);
          // Converte o valor JSON para TJSONArray
          SetLength(TValueArray, JsonArray.Count);
          // Define o tamanho do array com base na quantidade de itens do JSON

          // Obtém a classe dos elementos do array dinâmico
          ElemClass := (Prop.PropertyType as TRttiDynamicArrayType)
            .ElementType.AsInstance.MetaclassType;

          // Itera sobre os itens do array no JSON
          for I := 0 to JsonArray.Count - 1 do
          begin
            // Se o item do array no JSON for um objeto, processa recursivamente
            if JsonArray.Items[I] is TJSONObject then
            begin
              SubObject := ElemClass.Create;
              // Cria uma instância do tipo de elemento do array
              try
                // Chama recursivamente para preencher o subobjeto com o JSON
                JSONToObject(SubObject, JsonArray.Items[I] as TJSONObject);
                TValueArray[I] := TValue.From(SubObject);
                // Armazena o subobjeto no array
              except
                SubObject.Free; // Libera o subobjeto em caso de erro
                raise;
              end;
            end
            else
              raise Exception.CreateFmt
                ('Array item at index %d is not a valid JSON object.', [I]);
            // Erro se o item do array não for um objeto JSON
          end;

          // Converte o array de valores para um TValue e o atribui à propriedade do objeto
          ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
          Prop.SetValue(AObject, ArrayValue);
        end
        else
        begin
          // Lida com propriedades de tipos simples (strings, números, etc.)

          // Verifica se o valor do campo no JSON é null
          if JsonPair.JsonValue.Null then
            Prop.SetValue(AObject, TValue.Empty) // Atribui um valor vazio
          else if JsonPair.JsonValue is TJSONNumber then
          begin
            // Verifica se a propriedade é do tipo inteiro
            if Prop.PropertyType.Handle = TypeInfo(integer) then
              Prop.SetValue(AObject,
                TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
              // Verifica se a propriedade é do tipo double (decimal)
            else if Prop.PropertyType.Handle = TypeInfo(double) then
              Prop.SetValue(AObject,
                TValue.From<double>(TJSONNumber(JsonPair.JsonValue).AsDouble))
            else
              raise Exception.CreateFmt
                ('Tipo numérico não suportado para a propriedade "%s".',
                [Prop.Name]);
          end
          else if JsonPair.JsonValue is TJSONTrue then
            Prop.SetValue(AObject, TValue.From<Boolean>(True))
            // Atribui valor booleano True
          else if JsonPair.JsonValue is TJSONFalse then
            Prop.SetValue(AObject, TValue.From<Boolean>(False))
            // Atribui valor booleano False
          else if JsonPair.JsonValue is TJSONString then
          begin
            // Verifica se a propriedade é uma data
            if Prop.PropertyType.Handle = TypeInfo(TDate) then
              Prop.SetValue(AObject,
                TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue)
                .Value, False)))
              // Verifica se a propriedade é um DateTime
            else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
              Prop.SetValue(AObject,
                TValue.From<TDateTime>
                (ISO8601ToDate(TJSONString(JsonPair.JsonValue).Value, True)))
            else
              // Atribui o valor como string
              Prop.SetValue(AObject,
                TValue.From<string>(TJSONString(JsonPair.JsonValue).Value));
          end
          else
            raise Exception.CreateFmt
              ('Tipo de JSON não suportado para a propriedade "%s".',
              [Prop.Name]); // Erro se o tipo JSON não for suportado
        end;
      end;
    end;
  finally
    Context.Free; // Libera o contexto RTTI ao final
  end;
end;

procedure TJsonDynamicConverter.ObjectToJSON(AObject: TObject; var AJSON: TJSONObject);
var
  Context: TRttiContext; // Contexto para RTTI
  RttiType: TRttiType; // Tipo RTTI do objeto
  Prop: TRttiProperty; // Propriedade atual do objeto
  PropValue: TValue; // Valor da propriedade atual
  JsonValue: TJSONValue; // Valor JSON temporário (não utilizado neste código)
  SubJSON: TJSONObject; // Objeto JSON para propriedades que são objetos
  JsonArray: TJSONArray; // Array JSON para propriedades que são arrays dinâmicos
  DynArray: TRttiDynamicArrayType; // Tipo dinâmico da propriedade de array
  I: integer; // Contador de loop
  DynArrayValue: TValue; // Valor de cada elemento do array dinâmico
begin
  // Cria o contexto RTTI para acessar informações sobre o tipo do objeto
  Context := TRttiContext.Create;
  try
    // Obtém o tipo RTTI do objeto passado como parâmetro
    RttiType := Context.GetType(AObject.ClassType);

    // Itera pelas propriedades do objeto utilizando RTTI
    for Prop in RttiType.GetProperties do
    begin
      // Verifica se a propriedade é legível (possui método getter)
      if Prop.IsReadable then
      begin
        // Obtém o valor atual da propriedade
        PropValue := Prop.GetValue(AObject);

        // Lida com propriedades que são arrays dinâmicos
        if (Prop.PropertyType.TypeKind = tkDynArray) then
        begin
          // Obtém o tipo da propriedade como um array dinâmico
          DynArray := TRttiDynamicArrayType(Prop.PropertyType);
          // Cria um novo array JSON para armazenar os elementos do array dinâmico
          JsonArray := TJSONArray.Create;

          // Itera pelos elementos do array dinâmico
          for I := 0 to PropValue.GetArrayLength - 1 do
          begin
            // Obtém o valor de cada elemento do array
            DynArrayValue := PropValue.GetArrayElement(I);
            // Verifica se o elemento é um objeto
            if DynArrayValue.IsObject then
            begin
              // Cria um novo objeto JSON para o elemento
              SubJSON := TJSONObject.Create;
              // Chama recursivamente a função para converter o objeto em JSON
              ObjectToJSON(DynArrayValue.AsObject, SubJSON);
              // Adiciona o objeto JSON ao array JSON
              JsonArray.AddElement(SubJSON);
            end
            // Verifica se o elemento é um número inteiro
            else if DynArrayValue.Kind = tkInteger then
              JsonArray.AddElement(TJSONNumber.Create(DynArrayValue.AsInteger))
            // Verifica se o elemento é um número em ponto flutuante
            else if DynArrayValue.Kind = tkFloat then
              JsonArray.AddElement(TJSONNumber.Create(DynArrayValue.AsExtended))
            // Verifica se o elemento é uma string
            else if DynArrayValue.Kind = tkString then
              JsonArray.AddElement(TJSONString.Create(DynArrayValue.AsString));
          end;

          // Adiciona o array JSON à propriedade correspondente no objeto JSON final
          AJSON.AddPair(Prop.Name, JsonArray);
        end
        // Lida com propriedades que são objetos aninhados
        else if PropValue.IsObject then
        begin
          // Cria um novo objeto JSON para a propriedade aninhada
          SubJSON := TJSONObject.Create;
          // Chama recursivamente a função para converter o objeto em JSON
          ObjectToJSON(PropValue.AsObject, SubJSON);
          // Adiciona o objeto JSON aninhado ao objeto JSON final
          AJSON.AddPair(Prop.Name, SubJSON);
        end
        // Lida com propriedades que são valores simples
        else if PropValue.Kind = tkInteger then
          AJSON.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsInteger))
        else if PropValue.Kind = tkFloat then
          AJSON.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsExtended))
        else if PropValue.Kind = tkUString then
          AJSON.AddPair(Prop.Name, TJSONString.Create(PropValue.AsString))
        else if PropValue.Kind = tkEnumeration then
        begin
          // Lida com valores booleanos
          if PropValue.TypeInfo = TypeInfo(Boolean) then
            AJSON.AddPair(Prop.Name, TJSONBool.Create(PropValue.AsBoolean))
          else
            AJSON.AddPair(Prop.Name, TJSONString.Create(PropValue.ToString));
        end
        // Lida com registros (como TDate e TDateTime)
        else if PropValue.Kind = tkRecord then
        begin
          // Verifica se a propriedade é do tipo TDate e a converte
          if Prop.PropertyType.Handle = TypeInfo(TDate) then
            AJSON.AddPair(Prop.Name,
              TJSONString.Create(DateToISO8601(PropValue.AsType<TDate>, False)))
          // Verifica se a propriedade é do tipo TDateTime e a converte
          else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
            AJSON.AddPair(Prop.Name,
              TJSONString.Create(DateToISO8601(PropValue.AsType<TDateTime>, True)));
        end;
      end;
    end;
  finally
    // Libera o contexto RTTI
    Context.Free;
  end;
end;

end.
