using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace Pet {

  /// <summary>
  /// 
  /// </summary>
  [DataContract]
  public class Pet {
    /// <summary>
    /// Gets or Sets id
    /// </summary>
    [DataMember(Name="id", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "id")]
    public integer id { get; set; }

    /// <summary>
    /// Gets or Sets category
    /// </summary>
    [DataMember(Name="category", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "category")]
    public object category { get; set; }

    /// <summary>
    /// Gets or Sets name
    /// </summary>
    [DataMember(Name="name", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "name")]
    public string name { get; set; }

    /// <summary>
    /// Gets or Sets photoUrls
    /// </summary>
    [DataMember(Name="photourls", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "photourls")]
    public array photoUrls { get; set; }

    /// <summary>
    /// Gets or Sets tags
    /// </summary>
    [DataMember(Name="tags", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "tags")]
    public array tags { get; set; }

    /// <summary>
    /// pet status in the store
    /// </summary>
    /// <value>pet status in the store</value>
    [DataMember(Name="status", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "status")]
    public string status { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Pet {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  category: ").Append(category).Append("\n");
      sb.Append("  name: ").Append(name).Append("\n");
      sb.Append("  photoUrls: ").Append(photoUrls).Append("\n");
      sb.Append("  tags: ").Append(tags).Append("\n");
      sb.Append("  status: ").Append(status).Append("\n");
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
