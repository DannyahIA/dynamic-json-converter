unit nt_converter;

interface

uses
  DateUtils,
  System.Rtti,
  System.JSON,
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,

  (*FIREDAC*)
  Data.DB,
  FireDAC.Comp.Client;

type
  TJSONConverter = class;
  TDatasetConverter = class;
  TObjectConverter = class;

  TNTConverter = class
  private
    FJSONConverter: TJSONConverter;
    FDatasetConverter: TDatasetConverter;
    FObjectConverter: TObjectConverter;
  public
    property JSONConverter: TJSONConverter read FJSONConverter
      write FJSONConverter;
    property DatasetConverter: TDatasetConverter read FDatasetConverter
      write FDatasetConverter;
    property ObjectConverter: TObjectConverter read FObjectConverter
      write FObjectConverter;
  end;

  TJSONConverter = class
  private
    { --------------- UTILS --------------- }

    // Check Json Value Type
    class procedure CheckJSONValueType(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject);

    // Handle Dynamic Array
    class procedure HandleDynamicArray(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject); static;

    // Handle Object
    class procedure HandleObject(JsonPair: TJSONPair; Prop: TRttiProperty;
      AObject: TObject); static;

  public
    { --------------- JSON TO OBJECT --------------- }

    // JSONString to Object
    class procedure JSONStringToObject(AJSONString: string; AObject: TObject);

    // JSONObject to Object
    class procedure JSONToObject(AJSONObject: TJSONObject; AObject: TObject);

    // JSONString to ObjectList
    class procedure JSONStringToObjectList<T: class, constructor>
      (AJSONString: string; AObjectList: TObjectList<T>);

    // JSONArray to ObjectList
    class procedure JSONArrayToObjectList<T: class, constructor>
      (AJSONArray: TJSONArray; AObjectList: TObjectList<T>);

    { --------------- JSON TO DATASET --------------- }

    // JSONObject to Dataset
    class procedure JSONObjectToDataset(JSONObject: TJSONObject;
      DataSet: TFDMemTable);

    // JSONArray to Dataset
    class procedure JSONArrayToDataset(JSONArray: TJSONArray;
      DataSet: TFDMemTable);
  end;

  TDatasetConverter = class
  private

  public
    { --------------- DATASET TO JSON --------------- }

    // Dataset to JSONObject
    class procedure DatasetToJSONObject(ADataset: TFDMemTable; AJSONObject: TJSONObject);

    // Dataset to JSONArray
    class procedure DatasetToJSONArray(ADataset: TFDMemTable; AJSONArray: TJSONArray);

    { --------------- DATASET TO OBJECT --------------- }

    // Dataset to Object
    class procedure DatasetToObject(ADataSet: TFDMemTable; AObject: TObject);

    // Dataset to ObjectList
    class procedure DatasetToObjectList<T: class, constructor>(ADataSet: TFDMemTable; AObjectList: TObjectList<T>);

    // Dataset to ObjectArray
    class procedure DatasetToObjectArray<T: class>(ADataSet: TFDMemTable; AArray: TArray<T>);
  end;

  TObjectConverter = class
  private
    // Handle Dynamic Array to JSONO
    class procedure HandleDynamicArrayToJSON(Prop: TRttiProperty;
      PropValue: TValue; var AJSONObject: TJSONObject); static;

    // Handle Object to JSON
    class procedure HandleObjectToJSON(Prop: TRttiProperty; PropValue: TValue;
      var AJSONObject: TJSONObject); static;

    // Handle Simple Value to JSON
    class procedure HandleSimpleValueToJSON(Prop: TRttiProperty;
      PropValue: TValue; var AJSONObject: TJSONObject); static;
  public
    { --------------- OBJECT TO JSON --------------- }

    // Object to JSONObject
    class procedure ObjectToJSONObject(AObject: TObject;
      var AJSONObject: TJSONObject);

    // ObjectArray to JSONArray
    class procedure ObjectArrayToJSONArray<T: class>(AArray: TArray<T>;
      var AJSONArray: TJSONArray); static;

    // ObjectList to JSONArray
    class procedure ObjectListToJSONArray<T: class>(AObjectList: TObjectList<T>;
      var AJSONArray: TJSONArray);

    { --------------- OBJECT TO DATASET --------------- }

    // Object to Dataset
    class procedure ObjectToDataset(AObject: TObject; ADataSet: TDataSet);

    // ObjectArray to Dataset
    class procedure ObjectArrayToDataset<T: class>(AArray: TArray<T>; ADataSet: TDataSet);

    // ObjectList to Dataset
    class procedure ObjectListToDataset<T: class>(AObjectList: TObjectList<T>; ADataSet: TDataSet);
  end;

implementation

{ ----------------------------- JSON CONVERTER ----------------------------- }

{ --------------- JSON TO OBJECT --------------- }

{ JSONString to Object }
class procedure TJSONConverter.JSONStringToObject(AJSONString: string;
  AObject: TObject);
begin
  // Faz o parsing da string JSON para um objeto JSON (TJSONObject)
  var
  AJSONObject := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
  try
    // Chama a fun��o para converter o objeto JSON para o objeto Delphi
    JSONToObject(AJSONObject, AObject);
  finally
    // Libera a mem�ria do objeto JSON ap�s o uso
    AJSONObject.Free;
  end;
end;

{ JSONObject to Object }
class procedure TJSONConverter.JSONToObject(AJSONObject: TJSONObject;
  AObject: TObject);
var
  Context: TRttiContext; // Contexto RTTI para acessar as propriedades do objeto
  RttiType: TRttiType; // Representa o tipo do objeto usando RTTI
  Prop: TRttiProperty; // Propriedade RTTI para cada campo no objeto
  JsonPair: TJSONPair; // Representa um par de chave-valor no JSON
begin
  // Inicializa o contexto RTTI
  Context := TRttiContext.Create;
  try
    // Obt�m o tipo RTTI do objeto
    RttiType := Context.GetType(AObject.ClassType);

    // Itera sobre cada par chave-valor do JSON
    for JsonPair in AJSONObject do
    begin
      // Tenta obter a propriedade do objeto correspondente � chave do JSON
      Prop := RttiType.GetProperty(JsonPair.JSONString.Value);

      // Verifica se a propriedade existe no objeto
      if Assigned(Prop) then
      begin
        // Verifica se a propriedade � um array din�mico e o valor no JSON � um array
        if (Prop.PropertyType.TypeKind = tkDynArray) and
          (JsonPair.JsonValue is TJSONArray) then
          // Chama a fun��o para lidar com arrays din�micos
          HandleDynamicArray(JsonPair, Prop, AObject)
          // Verifica se o valor no JSON � um objeto aninhado
        else if JsonPair.JsonValue is TJSONObject then
          // Chama a fun��o para lidar com objetos aninhados
          HandleObject(JsonPair, Prop, AObject)
        else
          // Chama a fun��o para lidar com tipos simples (strings, n�meros, etc.)
          CheckJSONValueType(JsonPair, Prop, AObject);
      end;
    end;
  finally
    // Libera o contexto RTTI ao final
    Context.Free;
  end;
end;

{ JSONString to ObjectList }
class procedure TJSONConverter.JSONStringToObjectList<T>(AJSONString: string;
  AObjectList: TObjectList<T>);
var
  I: integer; // Vari�vel para o loop
  SubObject: T; // Vari�vel para armazenar cada objeto criado a partir do JSON
begin
  // Faz o parsing da string JSON para um array JSON (TJSONArray)
  var
  AJSONArray := TJSONObject.ParseJSONValue(AJSONString) as TJSONArray;
  try
    // Converte o array JSON em uma lista de objetos
    JSONArrayToObjectList<T>(AJSONArray, AObjectList);
  finally
    // Libera a mem�ria do array JSON ap�s o uso
    AJSONArray.Free;
  end;
end;

{ JSONArray to ObjectList }
class procedure TJSONConverter.JSONArrayToObjectList<T>(AJSONArray: TJSONArray;
  AObjectList: TObjectList<T>);
var
  I: integer; // Vari�vel para iterar pelos itens do array
  SubObject: T;
  // Vari�vel para armazenar o objeto criado para cada item do array
begin
  // Limpa a lista para garantir que estar� vazia antes de adicionar novos objetos
  AObjectList.Clear;
  // Itera sobre cada item do array JSON
  for I := 0 to AJSONArray.Count - 1 do
  begin
    // Verifica se o item atual do array � um objeto JSON (TJSONObject)
    if AJSONArray.Items[I] is TJSONObject then
    begin
      // Cria um novo objeto do tipo gen�rico T
      SubObject := T.Create;
      try
        // Converte o item JSON para o objeto Delphi utilizando JSONToObject
        JSONToObject(AJSONArray.Items[I] as TJSONObject, SubObject);
        // Adiciona o objeto convertido � lista de objetos
        AObjectList.Add(SubObject);
      except
        // Libera a mem�ria do objeto caso ocorra algum erro durante a convers�o
        SubObject.Free;
        // Relan�a a exce��o para o tratamento apropriado
        raise;
      end;
    end
    else
      // Lan�a uma exce��o se o item no array n�o for um TJSONObject
      raise Exception.CreateFmt
        ('Item no �ndice %d n�o � um objeto JSON v�lido.', [I]);
  end;
end;

{ --------------- UTILS --------------- }

{ HandleObject }
class procedure TJSONConverter.HandleObject(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
var
  SubObject: TObject; // Objeto auxiliar para instanciar a classe aninhada
begin
  // Cria uma inst�ncia do tipo do objeto aninhado
  SubObject := Prop.PropertyType.AsInstance.MetaclassType.Create;
  try
    // Chama recursivamente para preencher o subobjeto com o JSON
    JSONToObject(JsonPair.JsonValue as TJSONObject, SubObject);
    // Atribui o subobjeto � propriedade do objeto principal
    Prop.SetValue(AObject, SubObject);
  except
    // Libera o subobjeto em caso de erro
    SubObject.Free;
    raise;
  end;
end;

{ HandleDynamicArray }
class procedure TJSONConverter.HandleDynamicArray(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
var
  JSONArray: TJSONArray; // Array no JSON
  ElemClass: TClass; // Classe dos elementos do array din�mico
  I: integer; // Iterador do loop
  SubObject: TObject;
  // Objeto auxiliar para criar inst�ncias dos elementos do array
  TValueArray: TArray<TValue>;
  // Array de valores para os elementos do array din�mico
  ArrayValue: TValue; // Valor final que ser� atribu�do � propriedade do objeto
begin
  // Converte o valor JSON para TJSONArray
  JSONArray := TJSONArray(JsonPair.JsonValue);
  // Define o tamanho do array din�mico com base no tamanho do array JSON
  SetLength(TValueArray, JSONArray.Count);

  // Obt�m a classe dos elementos do array din�mico
  ElemClass := (Prop.PropertyType as TRttiDynamicArrayType)
    .ElementType.AsInstance.MetaclassType;

  // Itera sobre os itens do array no JSON
  for I := 0 to JSONArray.Count - 1 do
  begin
    // Verifica se o item do array no JSON � um objeto JSON
    if JSONArray.Items[I] is TJSONObject then
    begin
      // Cria uma inst�ncia do tipo de elemento do array
      SubObject := ElemClass.Create;
      try
        // Chama recursivamente para preencher o subobjeto com o JSON
        JSONToObject(JSONArray.Items[I] as TJSONObject, SubObject);
        // Armazena o subobjeto no array de valores
        TValueArray[I] := TValue.From(SubObject);
      except
        // Libera o subobjeto em caso de erro
        SubObject.Free;
        raise;
      end;
    end
    else
      // Erro se o item do array n�o for um objeto JSON
      raise Exception.CreateFmt
        ('Array item at index %d is not a valid JSON object.', [I]);
  end;

  // Converte o array de valores para um TValue e o atribui � propriedade do objeto
  ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
  Prop.SetValue(AObject, ArrayValue);
end;

{ CheckValueType }
class procedure TJSONConverter.CheckJSONValueType(JsonPair: TJSONPair;
  Prop: TRttiProperty; AObject: TObject);
begin
  // Verifica se o valor do campo no JSON � null
  if JsonPair.JsonValue.Null then
    // Atribui um valor vazio se o JSON contiver um valor nulo
    Prop.SetValue(AObject, TValue.Empty)
  else if JsonPair.JsonValue is TJSONNumber then
  begin
    // Verifica se a propriedade � um n�mero inteiro
    if Prop.PropertyType.Handle = TypeInfo(integer) then
      Prop.SetValue(AObject,
        TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
      // Verifica se a propriedade � um n�mero decimal
    else if Prop.PropertyType.Handle = TypeInfo(double) then
      Prop.SetValue(AObject, TValue.From<double>(TJSONNumber(JsonPair.JsonValue)
        .AsDouble))
    else
      // Gera um erro se o tipo num�rico n�o for suportado
      raise Exception.CreateFmt
        ('Tipo num�rico n�o suportado para a propriedade "%s".', [Prop.Name]);
  end
  else if JsonPair.JsonValue is TJSONTrue then
    // Atribui o valor booleano True
    Prop.SetValue(AObject, TValue.From<Boolean>(True))
  else if JsonPair.JsonValue is TJSONFalse then
    // Atribui o valor booleano False
    Prop.SetValue(AObject, TValue.From<Boolean>(False))
  else if JsonPair.JsonValue is TJSONString then
  begin
    // Verifica se a propriedade � uma data
    if Prop.PropertyType.Handle = TypeInfo(TDate) then
      Prop.SetValue(AObject,
        TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue)
        .Value, False)))
      // Verifica se a propriedade � um DateTime
    else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
      Prop.SetValue(AObject,
        TValue.From<TDateTime>(ISO8601ToDate(TJSONString(JsonPair.JsonValue)
        .Value, True)))
    else
      // Atribui o valor como string
      Prop.SetValue(AObject,
        TValue.From<string>(TJSONString(JsonPair.JsonValue).Value));
  end
  else
    // Gera um erro se o tipo JSON n�o for suportado
    raise Exception.CreateFmt
      ('Tipo de JSON n�o suportado para a propriedade "%s".', [Prop.Name]);
end;

{ --------------- JSON TO DATASET --------------- }

{ JSONArray to Dataset }
class procedure TJSONConverter.JSONArrayToDataset(JSONArray: TJSONArray;
  DataSet: TFDMemTable);
var
  I: integer;
  JSONObject: TJSONObject;
  Field: TField;
begin
  try
    DataSet.DisableControls;
    DataSet.Open;

    for I := 0 to JSONArray.Count - 1 do
    begin
      JSONObject := JSONArray.Items[I] as TJSONObject;

      DataSet.Append;

      for Field in DataSet.Fields do
      begin
        case Field.DataType of
          TFieldType.ftUnknown:
            Field.AsString := JSONObject.GetValue(Field.FieldName).Value;

          TFieldType.ftString:
            Field.AsString := JSONObject.GetValue(Field.FieldName).Value;

          TFieldType.ftInteger:
            Field.AsInteger :=
              StrToInt(JSONObject.GetValue(Field.FieldName).Value);

          TFieldType.ftBoolean:
            Field.AsBoolean :=
              StrToBool(JSONObject.GetValue(Field.FieldName).Value);

          TFieldType.ftFloat:
            Field.AsFloat :=
              StrToFloat(JSONObject.GetValue(Field.FieldName).Value);

          TFieldType.ftCurrency:
            Field.AsCurrency :=
              StrToFloat(JSONObject.GetValue(Field.FieldName).Value);

          TFieldType.ftDateTime, TFieldType.ftDate:
            if not JSONObject.GetValue(Field.FieldName).Value.IsEmpty then
              Field.AsDateTime :=
                ISO8601ToDate(JSONObject.GetValue(Field.FieldName).Value);

          TFieldType.ftByte:
            Field.AsString := JSONObject.GetValue(Field.FieldName).Value;
        end;
      end;

      DataSet.Post;
    end;

    DataSet.First;
  finally
    DataSet.EnableControls;
  end;
end;

{ JSONObject to Dataset }
class procedure TJSONConverter.JSONObjectToDataset(JSONObject: TJSONObject;
  DataSet: TFDMemTable);
var
  Field: TField;
begin
  try
    DataSet.DisableControls;
    DataSet.Open;

    DataSet.Append;

    for Field in DataSet.Fields do
    begin
      case Field.DataType of
        TFieldType.ftUnknown, TFieldType.ftString:
          Field.AsString := JSONObject.GetValue(Field.FieldName).Value;

        TFieldType.ftInteger:
          Field.AsInteger :=
            StrToInt(JSONObject.GetValue(Field.FieldName).Value);

        TFieldType.ftBoolean:
          Field.AsBoolean :=
            StrToBool(JSONObject.GetValue(Field.FieldName).Value);

        TFieldType.ftFloat, TFieldType.ftCurrency:
          Field.AsFloat :=
            StrToFloat(JSONObject.GetValue(Field.FieldName).Value);

        TFieldType.ftDateTime, TFieldType.ftDate:
          if not JSONObject.GetValue(Field.FieldName).Value.IsEmpty then
            Field.AsDateTime :=
              ISO8601ToDate(JSONObject.GetValue(Field.FieldName).Value);

        TFieldType.ftByte:
          Field.AsString := JSONObject.GetValue(Field.FieldName).Value;
      end;
    end;

    DataSet.Post;
    DataSet.First;

  finally
    DataSet.EnableControls;
  end;
end;

{ ----------------------------- DATASET CONVERTER ---------------------------- }

{ --------------- DATASET TO JSON --------------- }

{ Dataset to JSONObject }
class procedure TDatasetConverter.DatasetToJSONObject(ADataSet: TFDMemTable; AJSONObject: TJSONObject);
var
  Field: TField;
begin
  if ADataSet.Active and (ADataSet.RecordCount > 0) then
  begin
    ADataSet.First; // Come�a do primeiro registro

    // Itera sobre os campos do DataSet
    for Field in ADataSet.Fields do
    begin
      case Field.DataType of
        TFieldType.ftUnknown, TFieldType.ftString:
          AJSONObject.AddPair(Field.FieldName, Field.AsString);

        TFieldType.ftInteger:
          AJSONObject.AddPair(Field.FieldName, TJSONNumber.Create(Field.AsInteger));

        TFieldType.ftBoolean:
          AJSONObject.AddPair(Field.FieldName, TJSONBool.Create(Field.AsBoolean));

        TFieldType.ftFloat, TFieldType.ftCurrency:
          AJSONObject.AddPair(Field.FieldName, TJSONNumber.Create(Field.AsFloat));

        TFieldType.ftDateTime, TFieldType.ftDate:
          if Field.AsString <> '' then
            AJSONObject.AddPair(Field.FieldName, Field.AsString);

        TFieldType.ftByte:
          AJSONObject.AddPair(Field.FieldName, Field.AsString);
      end;
    end;
  end;
end;

{ Dataset to JSONArray }
class procedure TDatasetConverter.DatasetToJSONArray(ADataset: TFDMemTable; AJSONArray: TJSONArray);
var
  I: Integer;
  JSONObject: TJSONObject;
begin
  if ADataSet.Active and (ADataSet.RecordCount > 0) then
  begin
    ADataSet.First; // Come�a do primeiro registro

    // Itera sobre os registros do DataSet
    while not ADataSet.Eof do
    begin
      JSONObject := TJSONObject.Create;

      // Chama o m�todo para popular o JSONObject com os dados do DataSet
      DatasetToJSONObject(ADataSet, JSONObject);
      AJSONArray.AddElement(JSONObject); // Adiciona o objeto JSON ao array

      ADataSet.Next; // Avan�a para o pr�ximo registro
    end;
  end;
end;

{ --------------- DATASET TO OBJECT --------------- }

{ Dataset to Object }
class procedure TDatasetConverter.DatasetToObject(ADataSet: TFDMemTable; AObject: TObject);
var
  Field: TField;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
begin
  if ADataSet.Active and (ADataSet.RecordCount > 0) then
  begin
    ADataSet.First; // Come�a do primeiro registro

    // Obt�m o contexto RTTI e o tipo RTTI do objeto
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(AObject.ClassType);

      // Itera sobre os campos do DataSet
      for Field in ADataSet.Fields do
      begin
        // Tenta encontrar a propriedade correspondente no objeto
        RttiProp := RttiType.GetProperty(Field.FieldName);
        if Assigned(RttiProp) and RttiProp.IsWritable then
        begin
          case Field.DataType of
            TFieldType.ftString, TFieldType.ftWideString:
              RttiProp.SetValue(AObject, Field.AsString);

            TFieldType.ftInteger:
              RttiProp.SetValue(AObject, Field.AsInteger);

            TFieldType.ftBoolean:
              RttiProp.SetValue(AObject, Field.AsBoolean);

            TFieldType.ftFloat, TFieldType.ftCurrency:
              RttiProp.SetValue(AObject, Field.AsFloat);

            TFieldType.ftDateTime, TFieldType.ftDate:
              if Field.AsString <> '' then
                RttiProp.SetValue(AObject, ISO8601ToDate(Field.AsString));

            TFieldType.ftByte:
              RttiProp.SetValue(AObject, Field.AsString);
          end;
        end;
      end;
    finally
      RttiContext.Free;
    end;
  end;
end;

{ Dataset to ObjectList }
class procedure TDatasetConverter.DatasetToObjectList<T>(ADataSet: TFDMemTable; AObjectList: TObjectList<T>);
var
  I: Integer;
  AObject: T;
begin
  if ADataSet.Active and (ADataSet.RecordCount > 0) then
  begin
    ADataSet.First; // Come�a do primeiro registro

    while not ADataSet.Eof do
    begin
      AObject := T.Create; // Cria uma nova inst�ncia do objeto
      try
        DatasetToObject(ADataSet, AObject); // Popula o objeto com os dados do DataSet
        AObjectList.Add(AObject); // Adiciona o objeto � lista
      except
        AObject.Free; // Libera o objeto em caso de erro
        raise;
      end;

      ADataSet.Next; // Avan�a para o pr�ximo registro
    end;
  end;
end;

{ Dataset to ObjectArray }
class procedure TDatasetConverter.DatasetToObjectArray<T>(ADataSet: TFDMemTable; AArray: TArray<T>);
var
  ObjectList: TObjectList<T>;
begin
  ObjectList := TObjectList<T>.Create;
  try
    DatasetToObject(ADataSet, ObjectList); // Popula a lista de objetos
    AArray := ObjectList.ToArray; // Converte a lista em um array
  finally
    ObjectList.Free; // Libera a lista ap�s a convers�o
  end;
end;

{ ----------------------------- OBJECT CONVERTER ----------------------------- }

{ --------------- OBJECT TO JSON --------------- }

{ Object to JSONObject }
class procedure TObjectConverter.ObjectToJSONObject(AObject: TObject;
  var AJSONObject: TJSONObject);
var
  Context: TRttiContext; // Contexto RTTI para acessar metadados do objeto
  RttiType: TRttiType; // Tipo RTTI do objeto atual
  Prop: TRttiProperty; // Propriedade do objeto sendo iterada
  PropValue: TValue; // Valor da propriedade sendo processada
begin
  // Cria um contexto RTTI para acessar informa��es sobre o objeto
  Context := TRttiContext.Create;
  try
    // Obt�m o tipo RTTI do objeto passado (classe do objeto)
    RttiType := Context.GetType(AObject.ClassType);
    // Itera pelas propriedades do objeto utilizando RTTI
    for Prop in RttiType.GetProperties do
    begin
      // Verifica se a propriedade tem um m�todo getter (� leg�vel)
      if Prop.IsReadable then
      begin
        // Obt�m o valor atual da propriedade
        PropValue := Prop.GetValue(AObject);
        // Verifica se a propriedade � um array din�mico
        if Prop.PropertyType.TypeKind = tkDynArray then
          // Chama fun��o para lidar com arrays din�micos
          HandleDynamicArrayToJSON(Prop, PropValue, AJSONObject)
          // Verifica se a propriedade � um objeto
        else if PropValue.IsObject then
          // Chama fun��o para lidar com objetos aninhados
          HandleObjectToJSON(Prop, PropValue, AJSONObject)
          // Caso contr�rio, lida com propriedades simples (inteiros, strings, etc.)
        else
          HandleSimpleValueToJSON(Prop, PropValue, AJSONObject);
      end;
    end;
  finally
    // Libera o contexto RTTI ap�s o processamento
    Context.Free;
  end;
end;

{ ObjectList to JSONArray }
class procedure TObjectConverter.ObjectListToJSONArray<T>
  (AObjectList: TObjectList<T>; var AJSONArray: TJSONArray);
var
  I: integer;
  SubJSON: TJSONObject;
begin
  AJSONArray := TJSONArray.Create; // Cria o array JSON

  // Itera pela lista de objetos
  for I := 0 to AObjectList.Count - 1 do
  begin
    SubJSON := TJSONObject.Create; // Cria um novo objeto JSON para cada item
    ObjectToJSONObject(AObjectList[I], SubJSON); // Converte o objeto para JSON
    AJSONArray.AddElement(SubJSON); // Adiciona o objeto JSON ao array
  end;
end;

{ Array to JSONArray }
class procedure TObjectConverter.ObjectArrayToJSONArray<T>(AArray: TArray<T>;
  var AJSONArray: TJSONArray);
var
  I: integer;
  SubJSON: TJSONObject;
  Obj: TObject;
begin
  AJSONArray := TJSONArray.Create; // Cria o array JSON

  // Itera pelo array de objetos
  for I := 0 to Length(AArray) - 1 do
  begin
    // Converte o item do TArray para TObject
    Obj := TObject(AArray[I]);

    SubJSON := TJSONObject.Create; // Cria um novo objeto JSON para cada item

    // Converte o objeto para JSON
    ObjectToJSONObject(Obj, SubJSON);

    // Adiciona o objeto JSON ao array
    AJSONArray.AddElement(SubJSON);
  end;
end;

{ --------------- UTILS --------------- }

{ HandleObjectToJSON }
class procedure TObjectConverter.HandleObjectToJSON(Prop: TRttiProperty;
  PropValue: TValue; var AJSONObject: TJSONObject);
var
  SubJSON: TJSONObject;
  // Subobjeto JSON para armazenar o valor do objeto aninhado
begin
  // Cria um objeto JSON para armazenar o conte�do do objeto aninhado
  SubJSON := TJSONObject.Create;
  // Converte o objeto aninhado chamando recursivamente ObjectToJSONObject
  ObjectToJSONObject(PropValue.AsObject, SubJSON);
  // Adiciona o subobjeto JSON ao objeto JSON principal
  AJSONObject.AddPair(Prop.Name, SubJSON);
end;

{ HandleDynamicArrayToJSON }
class procedure TObjectConverter.HandleDynamicArrayToJSON(Prop: TRttiProperty;
  PropValue: TValue; var AJSONObject: TJSONObject);
var
  JSONArray: TJSONArray; // Array JSON para armazenar os valores
  I: integer; // Contador de loop para iterar sobre os elementos do array
  DynArrayValue: TValue; // Valor de cada elemento do array din�mico
  SubJSON: TJSONObject; // Subobjeto JSON para armazenar objetos dentro do array
begin
  // Cria um array JSON para armazenar os elementos do array din�mico
  JSONArray := TJSONArray.Create;
  // Itera pelos elementos do array din�mico
  for I := 0 to PropValue.GetArrayLength - 1 do
  begin
    // Obt�m o valor de cada elemento do array
    DynArrayValue := PropValue.GetArrayElement(I);
    // Verifica se o elemento do array � um objeto
    if DynArrayValue.IsObject then
    begin
      // Cria um objeto JSON para o elemento
      SubJSON := TJSONObject.Create;
      // Converte o objeto chamando recursivamente ObjectToJSONObject
      ObjectToJSONObject(DynArrayValue.AsObject, SubJSON);
      // Adiciona o objeto JSON ao array
      JSONArray.AddElement(SubJSON);
    end
    // Caso o elemento seja um inteiro
    else if DynArrayValue.Kind = tkInteger then
      JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsInteger))
      // Caso o elemento seja um n�mero de ponto flutuante
    else if DynArrayValue.Kind = tkFloat then
      JSONArray.AddElement(TJSONNumber.Create(DynArrayValue.AsExtended))
      // Caso o elemento seja uma string
    else if DynArrayValue.Kind = tkString then
      JSONArray.AddElement(TJSONString.Create(DynArrayValue.AsString));
  end;
  // Adiciona o array JSON ao objeto JSON principal
  AJSONObject.AddPair(Prop.Name, JSONArray);
end;

{ HandleSimpleValueToJSON }
class procedure TObjectConverter.HandleSimpleValueToJSON(Prop: TRttiProperty;
  PropValue: TValue; var AJSONObject: TJSONObject);
begin
  // Caso o valor seja um n�mero inteiro
  if PropValue.Kind = tkInteger then
    AJSONObject.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsInteger))
    // Caso o valor seja um n�mero de ponto flutuante
  else if PropValue.Kind = tkFloat then
    AJSONObject.AddPair(Prop.Name, TJSONNumber.Create(PropValue.AsExtended))
    // Caso o valor seja uma string
  else if PropValue.Kind = tkUString then
    AJSONObject.AddPair(Prop.Name, TJSONString.Create(PropValue.AsString))
    // Lida com valores de enumera��o (como booleanos)
  else if PropValue.Kind = tkEnumeration then
  begin
    // Caso seja um valor booleano
    if PropValue.TypeInfo = TypeInfo(Boolean) then
      AJSONObject.AddPair(Prop.Name, TJSONBool.Create(PropValue.AsBoolean))
    else
      AJSONObject.AddPair(Prop.Name, TJSONString.Create(PropValue.ToString));
  end
  // Lida com tipos de registro, como TDate e TDateTime
  else if PropValue.Kind = tkRecord then
  begin
    // Caso a propriedade seja do tipo TDate
    if Prop.PropertyType.Handle = TypeInfo(TDate) then
      AJSONObject.AddPair(Prop.Name,
        TJSONString.Create(DateToISO8601(PropValue.AsType<TDate>, False)))
      // Caso a propriedade seja do tipo TDateTime
    else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
      AJSONObject.AddPair(Prop.Name,
        TJSONString.Create(DateToISO8601(PropValue.AsType<TDateTime>, True)));
  end;
end;

{ --------------- OBJECT TO DATASET --------------- }

{ Object to Dataset }
class procedure TObjectConverter.ObjectToDataset(AObject: TObject; ADataSet: TDataSet);
var
  ctx: TRttiContext;
  rttiType: TRttiType;
  prop: TRttiProperty;
  field: TField;
  value: TValue;
begin
  // Verifica se o DataSet est� aberto
  if not ADataSet.Active then
    ADataSet.Open;

  // Cria um novo registro no DataSet
  ADataSet.Append;

  // Obt�m o contexto RTTI e o tipo RTTI do objeto
  ctx := TRttiContext.Create;
  try
    rttiType := ctx.GetType(AObject.ClassType);

    // Itera sobre as propriedades do objeto
    for prop in rttiType.GetProperties do
    begin
      // Verifica se a propriedade � leg�vel
      if prop.IsReadable then
      begin
        // Encontra o campo correspondente no DataSet pelo nome da propriedade
        field := ADataSet.FindField(prop.Name);
        if Assigned(field) then
        begin
          // Obt�m o valor da propriedade
          value := prop.GetValue(AObject);

          // Atribui o valor da propriedade ao campo correspondente no DataSet
          case field.DataType of
            TFieldType.ftString, TFieldType.ftWideString:
              if value.Kind = tkString then
                field.AsString := value.AsString
              else if value.Kind = tkUString then
                field.AsString := value.ToString; // Trata strings Unicode

            TFieldType.ftInteger:
              if value.Kind in [tkInteger, tkInt64] then
                field.AsInteger := value.AsInteger
              else
                raise Exception.CreateFmt
                  ('Tipo de dado incompat�vel para o campo %s: esperado Integer, recebido %s',
                  [field.fieldName, value.TypeInfo.Name]);

            TFieldType.ftFloat, TFieldType.ftCurrency:
              if value.Kind = tkFloat then
                field.AsFloat := value.AsExtended
              else
                raise Exception.CreateFmt
                  ('Tipo de dado incompat�vel para o campo %s: esperado Float, recebido %s',
                  [field.fieldName, value.TypeInfo.Name]);

            TFieldType.ftDateTime, TFieldType.ftDate:
              if value.Kind = tkFloat then
                field.AsDateTime := value.AsExtended
                // TDateTime � um valor float internamente
              else
                raise Exception.CreateFmt
                  ('Tipo de dado incompat�vel para o campo %s: esperado DateTime, recebido %s',
                  [field.fieldName, value.TypeInfo.Name]);

            // Adicione outros tipos de dados conforme necess�rio

          else
            raise Exception.CreateFmt('Tipo de campo n�o suportado: %s',
              [field.fieldName]);
          end;
        end;
      end;
    end;

    // Salva as altera��es no DataSet
    ADataSet.Post;
  finally
    ctx.Free;
  end;
end;

{ ObjectList to Dataset }
class procedure TObjectConverter.ObjectListToDataset<T>(AObjectList: TObjectList<T>; ADataSet: TDataSet);
var
  I: Integer;
begin
  // Verifica se o DataSet est� aberto
  if not ADataSet.Active then
    ADataSet.Open;
  // Itera sobre a lista de objetos
  for I := 0 to AObjectList.Count - 1 do
  begin
    ObjectToDataset(AObjectList[I], ADataSet);
  end;
end;

{ ObjectArray to Dataset }
class procedure TObjectConverter.ObjectArrayToDataset<T>(AArray: TArray<T>; ADataSet: TDataSet);
var
  I: Integer;
begin
  // Verifica se o DataSet est� aberto
  if not ADataSet.Active then
    ADataSet.Open;
  // Itera sobre o array de objetos
  for I := Low(AArray) to High(AArray) do
  begin
    ObjectToDataset(AArray[I], ADataSet);
  end;
end;

end.
