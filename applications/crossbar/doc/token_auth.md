/*
Section: Crossbar
Title: Token Authentication
Language: en-US
Version: 3.21
*/

Authentication tokens are generated using one of the authentication endpoints exposed by Crossbar. See [User Authentication](./user_authentication.md) and [API Authentication](./api_authentication.md) as examples of generating authentication tokens.

Once you have an authentication token, you can access various Crossbar resource endpoints to manipulate the system or your account (provided you have the access).

Authentication tokens refresh their pvt\_modified timestamp each time they are used in an API request. Once an authentication token's pvt\_modified timestamp has passed a configurable timeout (usually one hour), it is automatically cleaned up by the system and no longer valid.

## Token Restrictions

The authentication token can be created with restrictions on what resource URIs (and HTTP methods) can be accessed by the requestor. This payload is added to the authentication payload used in any of the authentication methods provided ([User](./user_authentication.md), [API](./api_authentication.md), etc).

For example, when creating an authentication token via [API key](./api_authentication.md), include the following object to restrict the resultant authentication token to read-only:

    {"data":{
        "api_key":"{API_KEY}"
        ,"restrictions":{
            "get":["#"]
        }
     }
    }

AMQP binding tokens are used (`#` and `*`) to denote wildcards. An example with more fine-grained restrictions:

    {"data":{
        "api_key":"{API_KEY}"
        ,"restrictions":{
            "get":[
                "accounts/{ACCOUNT_ID}/users"
                ,"accounts/{ACCOUNT_ID}/users/*"
                ,"accounts/{ACCOUNT_ID}/users/*/*"
            ]
            "put":[
                "accounts/{ACCOUNT_ID}/users"
            ]
            ,"post":[
                "accounts/{ACCOUNT_ID}/users/*"
            ]
            ,"delete":[
                "accounts/{ACCOUNT_ID}/users/*"
            ]
        }
     }
    }

This would restrict the authentication token to only be able to access {ACCOUNT_ID}'s users resource and perform all of the CRUD actions (as well as quickcall and channel listings for a user). We can simply this restrictions object by using `*` for the method and `#` to match any URI with `/users`:

    {"data":{
        "api_key":"{API_KEY}"
        ,"restrictions":{
            "*":["accounts/{ACCOUNT_ID}/users/#"]
        }
     }
    }

Here the `#` matches 0 or more segments after `/users`.

## System-configured Token Restrictions

System administrators can create restrictions for the different types of authentication methods and the different types of users, permitting access to certain APIs while locking down others.

### Locking down authentication methods

Depending on how an auth token is generated, it may be desired to apply restrictions differently (perhaps those coming to `/v2/user_auth` should not be allowed to access any APIs not related to their user).

The format takes the shape of

```JSON
{"cb_user_auth":{...}
 ,"cb_api_auth":{...}
 ,"cb_other_auth":{...}
 ,"_":{...}
}
```

Here, `"_"` is the catch-all auth method for any that aren't explicitly named.

### Locking down user\_auth tokens

When generating user\_auth restrictions, the placeholders `{ACCOUNT_ID}` and `{USER_ID}` are available and will be expanded to the actual values associated with the user.

The first level keys in the `cb_user_auth` object are the `priv_level` values ("admin", "user" by default). This permits per-priv\_level restrictions. `"_"` can be used for any unknown priv\_level.

```JSON
{"cb_user_auth":{
   "admin":{...}
   ,"user":{...}
   ,"_":{...}
 }
}
```

Per-priv\_level, restrictions follow the endpoint restrictions. See the [Restricting Endpoints](#restricing-endpoints) section.

### Locking down api_auth tokens

When generating api\_auth restrictions, the placeholders `{ACCOUNT_ID}` and `{API_KEY}` are available.

### Restricting Endpoints

Restricting endpoints
 So "users" would apply to URIs with "/users" in the path, "accounts" to "/accounts", etc. The restrictions work backwards on the URI - `/accounts/{ACCOUNT_ID}/users/{USER_ID}` would first check for restrictions on "users" and if nothing restricts the request, "accounts" would be checked. Let's take a look at an example:

```JSON
{"users":{"_":true}
 ,"accounts":{
    "{ACCOUNT_ID}":{"_":true"}
    ,"_":false
  }
 ,"_":false
}
```

In the above example, paths being parsed would be permitted when encountering the "users" portion of the path. The "accounts" portion would be subject to the `{ACCOUNT_ID}` restriction; any other account\_id would be denied. Finally, any other endpoint would be denied as well.

Concretely, if User A in Account 1 were to access "/v2/accounts/1/users/A", the request would be permitted. If the URI was "/v2/accounts/2/users", the request would be permitted when checking "users" but denied when checking "accounts" (as `{ACCOUNT_ID}` would expand to `1` and not match `2`).

#### Resellers

Kazoo allows resellers to access their sub-accounts. However, if `"_":false` occurs on the "accounts" object, most requests would be denied. Fortunately, there are reseller-specific macros that can be used.


`{CHILD_ID}`|Allows reseller accounts to access their child accounts (but not grandchild, great-grandchild, etc).
`{DESCENDANT_ID}`|Allows reseller accounts to access any of their sub-accounts, irrespective of the depth in the reseller's tree.
`{PARENT_ID}`|Allows the sub-account access to their parent account. This can be useful in limited situations

#### Unknown macros

Any macros used that aren't known to the processor act like wildcards.

#### Endpoint arguments

Some endpoints take more than just an ID-type parameter. Staying with "users", URIs could be encountered like `/accounts/{ACCOUNT_ID}/users/{USER_ID}/quickcall/+14155550000`. How could we allow `/users/{USER_ID}` but restrict quickcall URIs *and* not allow users to delete themselves *and* restrict creating new users?

```JSON
{"users":{
   "{USER_ID}":{
     "quickcall":{"_":false}
     ,"DELETE":false
     ,"_":true
   }
   ,"PUT":{"_":false}
   ,"_":true
 }
}
```

#### Token restrictions vs Endpoint restrictions

Token restrictions short-circuit processing by denying the request before the endpoint modules process it. Just because the request continues processing doesn't mean endpoint restrictions have been circumnavigated. For instance, one could define a token restriction `{"users":{"DELETE":true}` to allow deleting a collection of users. However, the users endpoint doesn't support the operation, so the client will still get a 405 since DELETE isn't allowed on `/users`.

#### Processing URIs

When processing URIs for restrictions, the endpoints and their arguments are processed right to left. Remember, URI parsing breaks `/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/quickcall/+14155550000` into `[{"users":["{USER_ID}", "quickcall", "+14155550000"]}, {"accounts":["{ACCOUNT_ID}"]}]`. Restrictions will walk the list in the following order:

1. check for the "users" key
    - check for the sub key "{USER\_ID}"
        - check for the sub key "{USER\_ID}"."quickcall"
            - check for the sub key "{USER\_ID}"."quickcall"."+14155550000"
                - check for the sub key "{USER\_ID}"."quickcall"."+14155550000"."{VERB}"
                - check for the sub key "{USER\_ID}"."quickcall"."+14155550000"."\_"
            - check for the sub key "{USER\_ID}"."quickcall"."{VERB}"
            - check for the sub key "{USER\_ID}"."quickcall"."\_"
        - check for the sub key "{USER\_ID}"."{VERB}"
        - check for the sub key "{USER\_ID}"."\_"
    - check for the sub key "{VERB}"
    - check for the sub key "\_"
2. If nothing has killed the request, check for the "accounts" key
    - check for the sub key "{ACCOUNT\_ID}"
        - check for the sub key "{ACCOUNT\_ID}"."{VERB}"
    - check for the sub key "{VERB}"
    - check for the sub key "\_"

## API Endpoint

URL segment: `/token_auth`

## Sample cURL Requests

### Delete an authentication token

If you'd like to invalidate an authentication token programmatically (versus letting the system expire the token), you can issue a `DELETE`:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/token_auth
