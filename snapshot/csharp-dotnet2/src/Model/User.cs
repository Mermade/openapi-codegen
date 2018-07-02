using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace User {

  /// <summary>
  /// 
  /// </summary>
  [DataContract]
  public class User {
    /// <summary>
    /// Gets or Sets id
    /// </summary>
    [DataMember(Name="id", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "id")]
    public integer id { get; set; }

    /// <summary>
    /// Gets or Sets username
    /// </summary>
    [DataMember(Name="username", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "username")]
    public string username { get; set; }

    /// <summary>
    /// Gets or Sets firstName
    /// </summary>
    [DataMember(Name="firstname", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "firstname")]
    public string firstName { get; set; }

    /// <summary>
    /// Gets or Sets lastName
    /// </summary>
    [DataMember(Name="lastname", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "lastname")]
    public string lastName { get; set; }

    /// <summary>
    /// Gets or Sets email
    /// </summary>
    [DataMember(Name="email", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "email")]
    public string email { get; set; }

    /// <summary>
    /// Gets or Sets password
    /// </summary>
    [DataMember(Name="password", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "password")]
    public string password { get; set; }

    /// <summary>
    /// Gets or Sets phone
    /// </summary>
    [DataMember(Name="phone", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "phone")]
    public string phone { get; set; }

    /// <summary>
    /// User Status
    /// </summary>
    /// <value>User Status</value>
    [DataMember(Name="userstatus", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "userstatus")]
    public integer userStatus { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class User {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  username: ").Append(username).Append("\n");
      sb.Append("  firstName: ").Append(firstName).Append("\n");
      sb.Append("  lastName: ").Append(lastName).Append("\n");
      sb.Append("  email: ").Append(email).Append("\n");
      sb.Append("  password: ").Append(password).Append("\n");
      sb.Append("  phone: ").Append(phone).Append("\n");
      sb.Append("  userStatus: ").Append(userStatus).Append("\n");
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
