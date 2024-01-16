/*
 * Linked List implementation in JavaScript
 * Copyright (c) 2009 Nicholas C. Zakas
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */


/**
 * A linked list implementation in JavaScript.
 * @class LinkedList
 * @constructor
 */
function LinkedList() {

    /**
     * The number of items in the list.
     * @property _length
     * @type int
     * @private
     */
    this._length = 0;
    
    /**
     * Pointer to first item in the list.
     * @property _head
     * @type Object
     * @private
     */
    this._head = null;
}

    /**
     * Appends some data to the end of the list. This method traverses
     * the existing list and places the value at the end in a new item.
     * @param {variant} data The data to add to the list.
     * @return {Void}
     * @method add
     */

/*c js:ll.(integer) -> undefined */
function add(data) { 
    
        //create a new item object, place data in
        var node = { 
                data: data, 
                next: null 
            },
            
            //used to traverse the structure
            current;
    
        //special case: no items in the list yet
        if (this._head === null){
            this._head = node;
        } else {
            current = this._head;
            
            while(current.next){
                current = current.next;
            }
           
            current.next = node;            
        }
        
        //don't forget to update the count
        this._length++;
    
    }

    /**
     * Retrieves the data in the given position in the list.
     * @param {int} index The zero-based index of the item whose value 
     *      should be returned.
     * @return {variant} The value in the "data" portion of the given item
     *      or null if the item doesn't exist.
     * @method item
     */

/*c js:ll.(length) -> (int or null) */
function item(index){
    
        //check for out-of-bounds values
        if (index > -1 && index < this._length){
            var current = this._head,
                i = 0;
                
            while(  index > i++){
                current = current.next;            
            }
        
            return current.data;
        } else {
            return null;
        }
    }    


    /**
     * Removes the item from the given location in the list.
     * @param {int} index The zero-based index of the item to remove.
     * @return {variant} The data in the given position in the list or null if
     *      the item doesn't exist.
     * @method remove
     */

/*c js:ll.(length) -> (integer or null) */
function remove(index){
    
        //check for out-of-bounds values
        if (index > (-1) && index < this._length){
        
            var current = this._head,
                previous,
                i = 0;
                
            //special case: removing first item
            if (index === 0){
                this._head = current.next;
            } else {
        
                //find the right location
                while(index > i++){
                    previous = current;
                    current = current.next;            
                }
            
                //skip over the item to remove
                previous.next = current.next;
            }
        
            //decrement the length
            this._length--;
        
            //return the value
            return current.data;            
        
        } else {
            return null;
        }
    
    }

    /**
     * Returns the number of items in the list.
     * @return {int} The number of items in the list.
     * @method size
     */


/*c js:ll.() -> int */
function size(){
        return this._length;
    }

    /**
     * Converts the list into an array.
     * @return {Array} An array containing all of the data in the list.
     * @method toArray
     */
/*c js:ll.() -> [integer] */
function toArray(){
        var result = [],
            current = this._head;
        
        while(current){
            result.push(current.data);
            current = current.next;
        }
        
        return result;
    }


    /**
     * Converts the list into a string representation.
     * @return {String} A string representation of the list.
     * @method toString
     */
/*c js:ll.() -> string */
function toString(){
        return this.toArray().toString();
}

LinkedList.prototype = {

    //restore constructor
    constructor: LinkedList,
    
    add: add,
    
 //   item: item,
    
 //   remove: remove,
    
    size: size,
    
    toArray: toArray,
    
    toString: toString
};


