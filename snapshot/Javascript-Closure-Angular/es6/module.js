import petApi from './resources/petApi';
import storeApi from './resources/storeApi';
import userApi from './resources/userApi';

let moduleName = 'Swagger Petstore'.toLowerCase().replace(/\s/g, '.');

export default angular
  .module(moduleName, [])
  .service('petApi', petApi)
    .service('storeApi', storeApi)
    .service('userApi', userApi)
  ;

