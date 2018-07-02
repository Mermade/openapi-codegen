<?php
/*
 * Order
 */
namespace IO.OpenAPI.Api;

/*
 * Order
 */
class Order {
    /* @var int $id  */
    private $id;
/* @var int $petId  */
    private $petId;
/* @var int $quantity  */
    private $quantity;
/* @var string $shipDate  */
    private $shipDate;
/* @var string $status Order Status */
    private $status;
/* @var bool $complete  */
    private $complete;
}
/*
 * Category
 */
namespace IO.OpenAPI.Api;

/*
 * Category
 */
class Category {
    /* @var int $id  */
    private $id;
/* @var string $name  */
    private $name;
}
/*
 * User
 */
namespace IO.OpenAPI.Api;

/*
 * User
 */
class User {
    /* @var int $id  */
    private $id;
/* @var string $username  */
    private $username;
/* @var string $firstName  */
    private $firstName;
/* @var string $lastName  */
    private $lastName;
/* @var string $email  */
    private $email;
/* @var string $password  */
    private $password;
/* @var string $phone  */
    private $phone;
/* @var int $userStatus User Status */
    private $userStatus;
}
/*
 * Tag
 */
namespace IO.OpenAPI.Api;

/*
 * Tag
 */
class Tag {
    /* @var int $id  */
    private $id;
/* @var string $name  */
    private $name;
}
/*
 * Pet
 */
namespace IO.OpenAPI.Api;

/*
 * Pet
 */
class Pet {
    /* @var int $id  */
    private $id;
/* @var struct{} $category  */
    private $category;
/* @var string $name  */
    private $name;
/* @var [100]string $photoUrls  */
    private $photoUrls;
/* @var [100]struct{} $tags  */
    private $tags;
/* @var string $status pet status in the store */
    private $status;
}
/*
 * ApiResponse
 */
namespace IO.OpenAPI.Api;

/*
 * ApiResponse
 */
class ApiResponse {
    /* @var int $code  */
    private $code;
/* @var string $Type  */
    private $Type;
/* @var string $message  */
    private $message;
}
