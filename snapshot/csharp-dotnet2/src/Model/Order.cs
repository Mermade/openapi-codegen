using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace Order {

  /// <summary>
  /// 
  /// </summary>
  [DataContract]
  public class Order {
    /// <summary>
    /// Gets or Sets id
    /// </summary>
    [DataMember(Name="id", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "id")]
    public integer id { get; set; }

    /// <summary>
    /// Gets or Sets petId
    /// </summary>
    [DataMember(Name="petid", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "petid")]
    public integer petId { get; set; }

    /// <summary>
    /// Gets or Sets quantity
    /// </summary>
    [DataMember(Name="quantity", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "quantity")]
    public integer quantity { get; set; }

    /// <summary>
    /// Gets or Sets shipDate
    /// </summary>
    [DataMember(Name="shipdate", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "shipdate")]
    public string shipDate { get; set; }

    /// <summary>
    /// Order Status
    /// </summary>
    /// <value>Order Status</value>
    [DataMember(Name="status", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "status")]
    public string status { get; set; }

    /// <summary>
    /// Gets or Sets complete
    /// </summary>
    [DataMember(Name="complete", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "complete")]
    public boolean complete { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Order {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  petId: ").Append(petId).Append("\n");
      sb.Append("  quantity: ").Append(quantity).Append("\n");
      sb.Append("  shipDate: ").Append(shipDate).Append("\n");
      sb.Append("  status: ").Append(status).Append("\n");
      sb.Append("  complete: ").Append(complete).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }

    /// <summary>
    /// Get the JSON string presentation of the object
    /// </summary>
    /// <returns>JSON string presentation of the object</returns>
    public string ToJson() {
      return JsonConvert.SerializeObject(this, Formatting.Indented);
    }

}
}
