### Procedure: `TSomeClass.JSONToObject`

This procedure maps the fields of a `TJSONObject` to the corresponding properties of a Delphi object (`TObject`). It uses **RTTI (Run-Time Type Information)** to dynamically access object properties and assign values from the JSON, supporting both simple types (like integers, strings, and dates) and complex types (like dynamic arrays).

#### Parameters:
- `AObject: TObject`: The object instance to which the JSON data will be mapped.
- `AJSON: TJSONObject`: The JSON object containing the data that will populate `AObject`.

---

### Detailed Step-by-Step Documentation:

1. **RTTI Context Initialization**
   ```pascal
   Context := TRttiContext.Create;
   ```
   - The **`TRttiContext`** is created to access metadata about the object at runtime. It enables us to retrieve information about the object's properties and methods.

2. **Iterating Through JSON Pairs**
   ```pascal
   for JsonPair in AJSON do
   ```
   - This loop iterates over each key-value pair in the provided `TJSONObject`. Each `JsonPair` consists of a key (`JsonString`) and a value (`JsonValue`).

3. **Get Object Property**
   ```pascal
   Prop := RttiType.GetProperty(JsonPair.JsonString.Value);
   ```
   - For each JSON key, the corresponding property of the object (`AObject`) is retrieved using RTTI. `RttiType` represents the class type of the object, and `GetProperty` looks for a matching property name.

4. **Handling Dynamic Arrays**
   ```pascal
   if (Prop.PropertyType.TypeKind = tkDynArray) and (JsonPair.JsonValue is TJSONArray) then
   ```
   - If the property is a dynamic array and the JSON value is a JSON array (`TJSONArray`), the procedure proceeds with handling arrays.

   - **Array Handling**:
     1. **Initialize the Array**:
        ```pascal
        SetLength(TValueArray, JsonArray.Count);
        ```
        The procedure creates an array (`TValueArray`) of `TValue` with the same size as the JSON array.

     2. **Determine Array Element Class**:
        ```pascal
        ElemClass := (Prop.PropertyType as TRttiDynamicArrayType).ElementType.AsInstance.MetaclassType;
        ```
        RTTI is used to retrieve the class of the elements in the dynamic array. This allows the procedure to dynamically create objects for each array element.

     3. **Populate Array**:
        ```pascal
        SubObject := ElemClass.Create;
        JSONToObject(SubObject, JsonArray.Items[I] as TJSONObject);
        TValueArray[I] := TValue.From(SubObject);
        ```
        For each item in the JSON array:
        - A new instance of the element class (`SubObject`) is created.
        - The procedure recursively calls `JSONToObject` to populate the nested object with JSON data.
        - The object is then added to `TValueArray` as a `TValue`.

     4. **Assign Array to Object Property**:
        ```pascal
        ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
        Prop.SetValue(AObject, ArrayValue);
        ```
        The dynamic array is converted to `TValue` and assigned to the property of the object (`AObject`).

5. **Handling Simple Types (Strings, Numbers, Booleans)**
   - If the property is not an array, the procedure handles simple data types like strings, integers, booleans, and dates.

   - **Null Values**:
     ```pascal
     if JsonPair.JsonValue.Null then
        Prop.SetValue(AObject, TValue.Empty);
     ```
     If the JSON value is null, the property is set to an empty value.

   - **Numbers**:
     ```pascal
     if Prop.PropertyType.Handle = TypeInfo(integer) then
        Prop.SetValue(AObject, TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
     ```
     If the property is of type `integer` or `double`, the JSON number is converted and assigned.

   - **Booleans**:
     ```pascal
     if JsonPair.JsonValue is TJSONTrue then
        Prop.SetValue(AObject, TValue.From<Boolean>(True))
     ```
     JSON `true` and `false` values are assigned as booleans.

   - **Strings**:
     ```pascal
     if JsonPair.JsonValue is TJSONString then
        Prop.SetValue(AObject, TValue.From<string>(TJSONString(JsonPair.JsonValue).Value))
     ```
     If the value is a string, it is directly assigned to the property.

   - **Dates**:
     ```pascal
     if Prop.PropertyType.Handle = TypeInfo(TDate) then
        Prop.SetValue(AObject, TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue).Value, False)))
     ```
     Strings that represent dates are converted to `TDate` or `TDateTime` using the `ISO8601ToDate` function and assigned.

6. **Unsupported Types Handling**
   - If the JSON value does not match a supported type, an exception is raised:
     ```pascal
     raise Exception.CreateFmt('Tipo de JSON não suportado para a propriedade "%s".', [Prop.Name]);
     ```

7. **RTTI Context Cleanup**
   ```pascal
   finally
     Context.Free;
   end;
   ```
   - The RTTI context is freed to release memory resources once the procedure completes.

---

### Key Points to Consider for Maintenance:
- **Recursion**: The function is recursive for nested JSON objects, making it capable of handling complex data structures.
- **Array Handling**: Dynamic arrays require special handling due to their complex nature. Ensure that `ElemClass` is correctly identified for all array elements.
- **Exception Handling**: The procedure includes exception handling to manage invalid JSON structures and unexpected property types.
- **Supported Types**: The current version supports integers, doubles, booleans, strings, dates, and dynamic arrays. If you need to support additional types, extend the type-checking logic in the appropriate sections.
- **RTTI Performance**: While RTTI is powerful, it may introduce performance overhead in large datasets. Consider optimizing if necessary.

By following this documentation, future modifications should be easier, as each section clearly explains its purpose and behavior.