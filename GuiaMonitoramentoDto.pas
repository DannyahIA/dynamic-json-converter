unit GuiaMonitoramentoDto;

interface

uses System.Generics.Collections;

type
  TGuiaFormaRemuneracao = class;
  TGuiaDiagnosticoCid10 = class;
  TGuiaDeclaracaoNascido = class;
  TGuiaDeclaracaoObito = class;
  TGuiaProcedimento = class;
  TGuiaItemPacote = class;

  // GUIA MONITORAMENTO
  TGuiaMonitoramento = class
  private
    FGuiaId: string;
    FRegistroAns: string;
    FTipoRegistroId: string;
    FVersaoTissPrestador: string;
    FFormaEnvioId: string;
    FPrestadorCnesId: string;
    FPrestadorIndicadorIdentificacaoId: string;
    FPrestadorCnpjCpf: string;
    FPrestadorMunicipioId: string;
    FBeneficiarioCns: string;
    FBeneficiarioCpf: string;
    FBeneficiarioSexoId: string;
    FBeneficiarioDataNascimento: TDate;
    FBeneficiarioMunicipioId: string;
    FBeneficiarioNumeroRegistroPlano: string;
    FTipoGuiaId: string;
    FOrigemGuiaId: string;
    FCompetencia: string;
    FNumeroGuiaPrestador: string;
    FNumeroGuiaOperadora: string;
    FIdentificadorReembolso: string;
    FIdentificacaoValorPreestabelecido: string;
    FGuiaSolicitacaoInternacao: string;
    FDataSolicitacao: TDate;
    FNumeroGuiaSpsadtPrincipal: string;
    FDataAutorizacao: TDate;
    FDataRealizacao: TDate;
    FDataProtocoloCobranca: TDate;
    FDataPagamento: TDate;
    FDataProcessamentoGuia: TDate;
    FDataFimPeriodo: TDate;
    FDataInicialFaturamento: TDate;
    FValorTotalInformado: Double;
    FValorProcessado: Double;
    FValorTotalPagoProcedimento: Double;
    FValorTotalDiaria: Double;
    FValorTotalTaxa: Double;
    FValorTotalMaterial: Double;
    FValorTotalOpme: Double;
    FValorTotalMedicamento: Double;
    FValorGlosaGuia: Double;
    FValorPagoGuia: Double;
    FValorPagoFornecedor: Double;
    FValorTotalPagoTabelaPropria: Double;
    FValorTotalPagoCoparticipacao: Double;
    FIndicacaoRecemNato: string;
    FIndicacaoAcidente: string;
    FCaraterAtendimento: string;
    FTipoInternacao: string;
    FRegimeInternacao: string;
    FSaudeOcupacional: string;
    FTipoFaturamento: string;
    FDiariaAcompanhante: integer;
    FDiariaUti: integer;
    FMotivoSaidaId: string;
    FCboExecutante: string;
    FTipoConsulta: string;
    FRegistroAnsOperadoraIntermediaria: string;
    FTipoAtendimentoIntermediarioId: string;
    FTipoAtendimentoId: string;
    FRegimeAtendimentoId: string;
    FFormasRemuneracaoId: string;
    FValorRemuneracaoId: Double;
    FStatusAnsId: string;
    FGuiaPendente: string;
    FStatusEnviaAns: string;
    FChaveAcesso: string;
    FFormasRemuneracao: TArray<TGuiaFormaRemuneracao>;
    FDiagnosticosCid10: TArray<TGuiaDiagnosticoCid10>;
    FDeclaracoesNascido: TArray<TGuiaDeclaracaoNascido>;
    FDeclaracoesObito: TArray<TGuiaDeclaracaoObito>;
    FProcedimentos: TArray<TGuiaProcedimento>;
  public
    property GuiaId: string read FGuiaId write FGuiaId;
    property RegistroAns: string read FRegistroAns write FRegistroAns;
    property TipoRegistroId: string read FTipoRegistroId write FTipoRegistroId;
    property VersaoTissPrestador: string read FVersaoTissPrestador
      write FVersaoTissPrestador;
    property FormaEnvioId: string read FFormaEnvioId write FFormaEnvioId;
    property PrestadorCnesId: string read FPrestadorCnesId
      write FPrestadorCnesId;
    property PrestadorIndicadorIdentificacaoId: string
      read FPrestadorIndicadorIdentificacaoId
      write FPrestadorIndicadorIdentificacaoId;
    property PrestadorCnpjCpf: string read FPrestadorCnpjCpf
      write FPrestadorCnpjCpf;
    property PrestadorMunicipioId: string read FPrestadorMunicipioId
      write FPrestadorMunicipioId;
    property BeneficiarioCns: string read FBeneficiarioCns
      write FBeneficiarioCns;
    property BeneficiarioCpf: string read FBeneficiarioCpf
      write FBeneficiarioCpf;
    property BeneficiarioSexoId: string read FBeneficiarioSexoId
      write FBeneficiarioSexoId;
    property BeneficiarioDataNascimento: TDate read FBeneficiarioDataNascimento
      write FBeneficiarioDataNascimento;
    property BeneficiarioMunicipioId: string read FBeneficiarioMunicipioId
      write FBeneficiarioMunicipioId;
    property BeneficiarioNumeroRegistroPlano: string
      read FBeneficiarioNumeroRegistroPlano
      write FBeneficiarioNumeroRegistroPlano;
    property TipoGuiaId: string read FTipoGuiaId write FTipoGuiaId;
    property OrigemGuiaId: string read FOrigemGuiaId write FOrigemGuiaId;
    property Competencia: string read FCompetencia write FCompetencia;
    property NumeroGuiaPrestador: string read FNumeroGuiaPrestador
      write FNumeroGuiaPrestador;
    property NumeroGuiaOperadora: string read FNumeroGuiaOperadora
      write FNumeroGuiaOperadora;
    property IdentificadorReembolso: string read FIdentificadorReembolso
      write FIdentificadorReembolso;
    property IdentificacaoValorPreestabelecido: string
      read FIdentificacaoValorPreestabelecido
      write FIdentificacaoValorPreestabelecido;
    property GuiaSolicitacaoInternacao: string read FGuiaSolicitacaoInternacao
      write FGuiaSolicitacaoInternacao;
    property DataSolicitacao: TDate read FDataSolicitacao
      write FDataSolicitacao;
    property NumeroGuiaSpsadtPrincipal: string read FNumeroGuiaSpsadtPrincipal
      write FNumeroGuiaSpsadtPrincipal;
    property DataAutorizacao: TDate read FDataAutorizacao
      write FDataAutorizacao;
    property DataRealizacao: TDate read FDataRealizacao write FDataRealizacao;
    property DataProtocoloCobranca: TDate read FDataProtocoloCobranca
      write FDataProtocoloCobranca;
    property DataPagamento: TDate read FDataPagamento write FDataPagamento;
    property DataProcessamentoGuia: TDate read FDataProcessamentoGuia
      write FDataProcessamentoGuia;
    property DataFimPeriodo: TDate read FDataFimPeriodo write FDataFimPeriodo;
    property DataInicialFaturamento: TDate read FDataInicialFaturamento
      write FDataInicialFaturamento;
    property ValorTotalInformado: Double read FValorTotalInformado
      write FValorTotalInformado;
    property ValorProcessado: Double read FValorProcessado
      write FValorProcessado;
    property ValorTotalPagoProcedimento: Double read FValorTotalPagoProcedimento
      write FValorTotalPagoProcedimento;
    property ValorTotalDiaria: Double read FValorTotalDiaria
      write FValorTotalDiaria;
    property ValorTotalTaxa: Double read FValorTotalTaxa write FValorTotalTaxa;
    property ValorTotalMaterial: Double read FValorTotalMaterial
      write FValorTotalMaterial;
    property ValorTotalOpme: Double read FValorTotalOpme write FValorTotalOpme;
    property ValorTotalMedicamento: Double read FValorTotalMedicamento
      write FValorTotalMedicamento;
    property ValorGlosaGuia: Double read FValorGlosaGuia write FValorGlosaGuia;
    property ValorPagoGuia: Double read FValorPagoGuia write FValorPagoGuia;
    property ValorPagoFornecedor: Double read FValorPagoFornecedor
      write FValorPagoFornecedor;
    property ValorTotalPagoTabelaPropria: Double
      read FValorTotalPagoTabelaPropria write FValorTotalPagoTabelaPropria;
    property ValorTotalPagoCoparticipacao: Double
      read FValorTotalPagoCoparticipacao write FValorTotalPagoCoparticipacao;
    property IndicacaoRecemNato: string read FIndicacaoRecemNato
      write FIndicacaoRecemNato;
    property IndicacaoAcidente: string read FIndicacaoAcidente
      write FIndicacaoAcidente;
    property CaraterAtendimento: string read FCaraterAtendimento
      write FCaraterAtendimento;
    property TipoInternacao: string read FTipoInternacao write FTipoInternacao;
    property RegimeInternacao: string read FRegimeInternacao
      write FRegimeInternacao;
    property SaudeOcupacional: string read FSaudeOcupacional
      write FSaudeOcupacional;
    property TipoFaturamento: string read FTipoFaturamento
      write FTipoFaturamento;
    property DiariaAcompanhante: integer read FDiariaAcompanhante
      write FDiariaAcompanhante;
    property DiariaUti: integer read FDiariaUti write FDiariaUti;
    property MotivoSaidaId: string read FMotivoSaidaId write FMotivoSaidaId;
    property CboExecutante: string read FCboExecutante write FCboExecutante;
    property TipoConsulta: string read FTipoConsulta write FTipoConsulta;
    property RegistroAnsOperadoraIntermediaria: string
      read FRegistroAnsOperadoraIntermediaria
      write FRegistroAnsOperadoraIntermediaria;
    property TipoAtendimentoIntermediarioId: string
      read FTipoAtendimentoIntermediarioId
      write FTipoAtendimentoIntermediarioId;
    property TipoAtendimentoId: string read FTipoAtendimentoId
      write FTipoAtendimentoId;
    property RegimeAtendimentoId: string read FRegimeAtendimentoId
      write FRegimeAtendimentoId;
    property FormasRemuneracaoId: string read FFormasRemuneracaoId
      write FFormasRemuneracaoId;
    property ValorRemuneracaoId: Double read FValorRemuneracaoId
      write FValorRemuneracaoId;
    property StatusAnsId: string read FStatusAnsId write FStatusAnsId;
    property GuiaPendente: string read FGuiaPendente write FGuiaPendente;
    property StatusEnviaAns: string read FStatusEnviaAns write FStatusEnviaAns;
    property ChaveAcesso: string read FChaveAcesso write FChaveAcesso;
    property FormasRemuneracao: TArray<TGuiaFormaRemuneracao>
      read FFormasRemuneracao write FFormasRemuneracao;
    property DiagnosticosCid10: TArray<TGuiaDiagnosticoCid10>
      read FDiagnosticosCid10 write FDiagnosticosCid10;
    property DeclaracoesNascido: TArray<TGuiaDeclaracaoNascido>
      read FDeclaracoesNascido write FDeclaracoesNascido;
    property DeclaracoesObito: TArray<TGuiaDeclaracaoObito>
      read FDeclaracoesObito write FDeclaracoesObito;
    property Procedimentos: TArray<TGuiaProcedimento> read FProcedimentos
      write FProcedimentos;

    constructor Create;
  end;

  // FORMA REMUNERAÇÃO
  TGuiaFormaRemuneracao = class
  private
    FGuiaFormaRemuneracaoId: string;
    FGuiaId: string;
    FFormaRemuneracaoId: string;
    FValorRemuneracao: Double;
  public
    property GuiaFormaRemuneracaoId: string read FGuiaFormaRemuneracaoId
      write FGuiaFormaRemuneracaoId;
    property GuiaId: string read FGuiaId write FGuiaId;
    property FormaRemuneracaoId: string read FFormaRemuneracaoId
      write FFormaRemuneracaoId;
    property ValorRemuneracao: Double read FValorRemuneracao
      write FValorRemuneracao;
  end;

  // DIAGNÓSTICO CID 10
  TGuiaDiagnosticoCid10 = class
  private
    FCid10Id: string;
    FSequencia: integer;
  public
    property Cid10Id: string read FCid10Id write FCid10Id;
    property Sequencia: integer read FSequencia write FSequencia;
  end;

  // DECLARAÇÃO NASCIDO
  TGuiaDeclaracaoNascido = class
  private
    FDeclaracaoNascidoId: string;
    FGuiaId: string;
    FSequencia: integer;
    FNumeroDeclaracao: string;
  public
    property DeclaracaoNascidoId: string read FDeclaracaoNascidoId
      write FDeclaracaoNascidoId;
    property GuiaId: string read FGuiaId write FGuiaId;
    property Sequencia: integer read FSequencia write FSequencia;
    property NumeroDeclaracao: string read FNumeroDeclaracao
      write FNumeroDeclaracao;
  end;

  // DECLARAÇÃO ÓBITO
  TGuiaDeclaracaoObito = class
  private
    FDeclaracaoObitoId: string;
    FGuiaId: string;
    FSequencia: integer;
    FNumeroDeclaracao: string;
  public
    property DeclaracaoObitoId: string read FDeclaracaoObitoId
      write FDeclaracaoObitoId;
    property GuiaId: string read FGuiaId write FGuiaId;
    property Sequencia: integer read FSequencia write FSequencia;
    property NumeroDeclaracao: string read FNumeroDeclaracao
      write FNumeroDeclaracao;
  end;

  // PROCEDIMENTO
  TGuiaProcedimento = class
  private
    FProcedimentoId: string;
    FGuiaId: string;
    FSequencia: integer;
    FCodigoTabelaMonitorId: string;
    FGrupoProcedimentoId: string;
    FCodigoProcedimentoId: string;
    FDenteId: string;
    FRegiaoBocaId: string;
    FDenteFaceId: string;
    FQuantidadeInformada: Double;
    FValorInformado: Double;
    FQuantidadePaga: Double;
    FUnidadeMedidaId: string;
    FValorPagoProcedimento: Double;
    FValorPagoFornecedor: Double;
    FCnpjFornecedor: string;
    FValorCoparticipacao: Double;
    FItemsPacote: TArray<TGuiaItemPacote>;
  public
    property ProcedimentoId: string read FProcedimentoId write FProcedimentoId;
    property GuiaId: string read FGuiaId write FGuiaId;
    property Sequencia: integer read FSequencia write FSequencia;
    property GrupoProcedimentoId: string read FGrupoProcedimentoId
      write FGrupoProcedimentoId;
    property CodigoProcedimentoId: string read FCodigoProcedimentoId
      write FCodigoProcedimentoId;
    property CodigoTabelaMonitorId: string read FCodigoTabelaMonitorId
      write FCodigoTabelaMonitorId;
    property DenteId: string read FDenteId write FDenteId;
    property RegiaoBocaId: string read FRegiaoBocaId write FRegiaoBocaId;
    property DenteFaceId: string read FDenteFaceId write FDenteFaceId;
    property QuantidadeInformada: Double read FQuantidadeInformada
      write FQuantidadeInformada;
    property ValorInformado: Double read FValorInformado write FValorInformado;
    property QuantidadePaga: Double read FQuantidadePaga write FQuantidadePaga;
    property UnidadeMedidaId: string read FUnidadeMedidaId
      write FUnidadeMedidaId;
    property ValorPagoProcedimento: Double read FValorPagoProcedimento
      write FValorPagoProcedimento;
    property ValorPagoFornecedor: Double read FValorPagoFornecedor
      write FValorPagoFornecedor;
    property CnpjFornecedor: string read FCnpjFornecedor write FCnpjFornecedor;
    property ValorCoparticipacao: Double read FValorCoparticipacao
      write FValorCoparticipacao;
    property ItemsPacote: TArray<TGuiaItemPacote> read FItemsPacote
      write FItemsPacote;

    constructor Create;
  end;

  // ITEM PACOTE
  TGuiaItemPacote = class
  private
    FItemPacoteId: string;
    FGuiaProcedimentoId: string;
    FCodigoTabelaPacoteId: string;
    FCodigoProcedimento: string;
    FQuantidade: Double;
  public
    property ItemPacoteId: string read FItemPacoteId write FItemPacoteId;
    property GuiaProcedimentoId: string read FGuiaProcedimentoId
      write FGuiaProcedimentoId;
    property CodigoTabelaPacoteId: string read FCodigoTabelaPacoteId
      write FCodigoTabelaPacoteId;
    property CodigoProcedimento: string read FCodigoProcedimento
      write FCodigoProcedimento;
    property Quantidade: Double read FQuantidade write FQuantidade;
  end;

implementation

{ TGuiaMonitoramento }

constructor TGuiaMonitoramento.Create;
begin
  inherited;
end;

{ TGuiaProcedimento }

constructor TGuiaProcedimento.Create;
begin
  inherited;
end;

end.
