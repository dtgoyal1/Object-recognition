package pplAssignment

object  F2015B2A70848P{
    //Start Coding from here
    def listConcat[A](list : List[List[A]]):List[A]={
	  if(list.nonEmpty){
			list.head:::listConcat(list.tail)
	  }else Nil
  }                                                 //> listConcat: [A](list: List[List[A]])List[A]

	def findMaxMin[A](list : List[A],ans : A,fun:(A,A) => Boolean):A = {
		if(list.nonEmpty){
			if(fun(list.head,ans)){
				findMaxMin(list.tail,list.head,fun)
			}else{
				findMaxMin(list.tail,ans,fun)
			}
		}
		else{
			ans
		}
	}                                         //> findMaxMin: [A](list: List[A], max: A, greater: (A, A) => Boolean)A
	
  def dotProduct(matrix_1: List[List[Double]], matrix_2: List[List[Double]]): Double = {
  	val l1 = listConcat(matrix_1)
  	val l2 = listConcat(matrix_2)
  	
  	def dotProductRow(matrix_1: List[Double], matrix_2: List[Double]): Double = {
      if (matrix_1.nonEmpty){
      	matrix_1.head*matrix_2.head + dotProductRow(matrix_1.tail,matrix_2.tail)
      }else 0.0
 		}
 		dotProductRow(l1,l2)
 	}                                         //> dotProduct: (matrix_1: List[List[Double]], matrix_2: List[List[Double]])Doub
                                                  //| le
  def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int] ):List[List[Double]] ={
		
		def createNewRow(Image:List[Double],res:List[Double],size:Int):List[Double]={
				if(size>0){
				  createNewRow(Image.tail,res:::List(Image.head),size-1)
				}
				else{
				  res
				}
		}
		
		def nextKernel(Image:List[List[Double]],res:List[List[Double]],dotList: List[List[Double]]):List[List[Double]]={
			if(Image.nonEmpty){
				val newrow = createNewRow(Image.head,List(),kernelSize.tail.head)
				val currow = res.tail:::List(newrow)
				nextKernel(Image.tail, currow,dotList:::List(List(dotProduct(currow,Kernel))))
			}
			else dotList
		}
		
		def createFirstKernel(Image:List[List[Double]],res:List[List[Double]],size:List[Int]):List[List[Double]]={
			val row = size.head
			val col = size.tail.head
			
			if(row>0){
				val newrow = createNewRow(Image.head,List(),col)
				createFirstKernel(Image.tail,res:::List(newrow),List(row-1,col))
			}
			else{
				nextKernel(Image,res.tail,List(List(dotProduct(res.tail,Kernel))))
			}
		}
		
		def removeFirstColumn(Image: List[List[Double]],res: List[List[Double]]):List[List[Double]]={
			if(Image.nonEmpty){
				removeFirstColumn(Image.tail,res:::List(Image.head.tail))
			}
			else res.tail
		}
		
		def merge(dotList: List[List[Double]],tempdot: List[List[Double]],res: List[List[Double]]):List[List[Double]] ={
			if(dotList.nonEmpty){
				merge(dotList.tail,tempdot.tail,res:::List(dotList.head:::tempdot.head))
			}
			else res.tail
		}
		
		def nextColumn(Image: List[List[Double]],dotList:List[List[Double]],len : Int):List[List[Double]] = {
			if(len > 0){
				val newImage = removeFirstColumn(Image,List(List()))
				val tempdot = createFirstKernel(newImage,List(List()),kernelSize)
				val newdotList = merge(dotList,tempdot,List(List()))
				nextColumn(newImage,newdotList,len-1)
			}
			else dotList
		}
		
		val len = imageSize.tail.head - kernelSize.tail.head
		val dot = createFirstKernel(Image,List(List()),kernelSize)
		nextColumn(Image,dot,len)
  }                                                 //> convolute: (Image: List[List[Double]], Kernel: List[List[Double]], imageSiz
                                                  //| e: List[Int], kernelSize: List[Int])List[List[Double]]
 
  def activationLayer(activationFunc: Double => Double, Image : List[List[Double]]):List[List[Double]]={
  	
  	def activationRow(activationFunc: Double => Double, Image : List[Double]):List[Double]={
  	if (Image.nonEmpty){
  			List(activationFunc(Image.head)):::activationRow(activationFunc,Image.tail)
  		}else Nil
  	}
  
  	if (Image.nonEmpty){
  		List(activationRow(activationFunc, Image.head)):::activationLayer(activationFunc,Image.tail)
  	}else Nil
  }                                               //> activationLayer: (activationFunc: Double => Double, Image: List[List[Double
                                                  //| ]])List[List[Double]]
  
  def normalise(Image:List[List[Double]]):List[List[Int]]={
  	val linearImage  = listConcat(Image)
  	val maxi = findMaxMin[Double](linearImage,-10000000.0,(p:Double,q:Double)=>p>q)
  	val mini = findMaxMin[Double](linearImage,10000000.0,(p:Double,q:Double)=>p<q)
  	
  	def normaliseRow(normFunc: Double => Int, Image : List[Double]):List[Int]={
  	if (Image.nonEmpty){
  			List(normFunc(Image.head)):::normaliseRow(normFunc,Image.tail)
  		}else Nil
    }
    def normaliseHelper(normFunc: Double => Int, Image : List[List[Double]]):List[List[Int]]={
  	  if (Image.nonEmpty){
  		  List(normaliseRow(normFunc, Image.head)):::normaliseHelper(normFunc,Image.tail)
  		}else Nil
    }
  
    normaliseHelper((p:Double)=>(((p-mini)/(maxi-mini))*255).round.toInt,Image)
  
  }                                               //> normalise: (Image: List[List[Double]])List[List[Int]]
  
  def singlePooling(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int):List[Double]={
  	
  	def createNewRow(Image:List[Double],res:List[Double],size:Int):List[Double]={
				if(size>0){
					createNewRow(Image.tail,res:::List(Image.head),size-1)
				}
				else{ 
				res
				}
		}
  	
  	def poolRow(Image:List[List[Double]],res:List[List[Double]],size:List[Int]):List[List[Double]]={
			val row = size.head
			val col = size.tail.head
			if(row>0){
				val newrow = createNewRow(Image.head,List(),col)
				poolRow(Image.tail,res:::List(newrow),List(row-1,col))
			}
			else{
				res.tail
			}
		}
		
		def removeFirstColumn(Image: List[List[Double]],res: List[List[Double]]):List[List[Double]]={
			if(Image.nonEmpty){
				removeFirstColumn(Image.tail,res:::List(Image.head.tail))
			}
			else res.tail
		}
			
  	def removenColumn(Image: List[List[Double]],n: Int):List[List[Double]]={
			if(n>0){
				val temp = removeFirstColumn(Image,List(List()))
				removenColumn(temp,n-1)
			}
			else{
			Image
			}
		}
  	
  	def singlePoolingHelper(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int, res:List[Double]):List[Double]={
			if(Image.head.nonEmpty){
					val temp = poolRow(Image,List(List()),List(K,K))
					val newImage = removenColumn(Image,K)
					val newres = res:::List(poolingFunc(listConcat(temp)))
					singlePoolingHelper(poolingFunc, newImage,K,newres)
			}
  		else{
  		res
  		}
  	}
  	singlePoolingHelper(poolingFunc, Image, K, List())
  }                                               //> singlePooling: (poolingFunc: List[Double] => Double, Image: List[List[Doubl
                                                  //| e]], K: Int)List[Double]
  
  def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int ):List[List[Double]] ={
  	
  	def poolingLayerHelper(Image:List[List[Double]],res:List[List[Double]], size:Int,amp: List[List[Double]]):List[List[Double]]={
  		if(size > 0){
  			poolingLayerHelper(Image.tail,res:::List(Image.head), size-1,amp)
  		}
  		else{
  			val ans = singlePooling(poolingFunc,res.tail,K)
  			if(Image.nonEmpty){
  				poolingLayerHelper(Image,List(List()),K,amp:::List(ans))
  			}else{
  			(amp:::List(ans)).tail
  			}
  		}
  	}
  	poolingLayerHelper(Image,List(List()),K,List(List()))
  }                                               //> poolingLayer: (poolingFunc: List[Double] => Double, Image: List[List[Double
                                                  //| ]], K: Int)List[List[Double]]
  
  def mixedLayer(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int], activationFunc:Double => Double, poolingFunc:List[Double]=>Double,  K:Int):List[List[Double]] = {
  	val convolutedImage = convolute(Image, Kernel, imageSize, kernelSize)
  	val activatedImage = activationLayer(activationFunc,convolutedImage)
  	val pooledImage = poolingLayer(poolingFunc,activatedImage,K)
  	pooledImage
  }                                               //> mixedLayer: (Image: List[List[Double]], Kernel: List[List[Double]], imageSi
                                                  //| ze: List[Int], kernelSize: List[Int], activationFunc: Double => Double, poo
                                                  //| lingFunc: List[Double] => Double, K: Int)List[List[Double]]
  
  def assembly(Image:List[List[Double]], imageSize:List[Int], w1:Double, w2:Double, b:Double, Kernel1:List[List[Double]], kernelSize1:List[Int], Kernel2:List[List[Double]], kernelSize2:List[Int], Kernel3:List[List[Double]], kernelSize3:List[Int], Size: Int):List[List[Int]] = {
  	
  	def ReLu(x:Double):Double={
  		if(x>0)x
  		else 0
  	}
  	
  	def l_ReLu(x:Double):Double={
  		if(x>0)x
  		else 0.5*x
  	}
  	
  	def maxpool(x:List[Double]):Double={
  		findMaxMin[Double](x,-1000.0,(p:Double,q:Double)=>p>q)
  	}
  	
  	def avgpool(x:List[Double]):Double={
  		def findAvg(x:List[Double],sum:Double,count:Integer):Double={
  			if(x.nonEmpty){
  				findAvg(x.tail,sum + x.head,count + 1)
  			}
  			else sum/count
  		}
  		findAvg(x,0.0,0)
  	}
  	
  	def add(a: List[List[Double]],b:List[List[Double]]):List[List[Double]]={
  		def addRow(Image1 : List[Double], Image2 : List[Double]):List[Double]={
  			if (Image1.nonEmpty){
  				List(Image1.head+Image2.head):::addRow(Image1.tail,Image2.tail)
  			}else Nil
  		}
  		def addHelper(Image1 : List[List[Double]], Image2 : List[List[Double]]):List[List[Double]]={
  		if (Image1.nonEmpty){
  			List(addRow(Image1.head, Image2.head)):::addHelper(Image1.tail,Image2.tail)
  		}else Nil
  		}
  		addHelper(a,b)
  	}
  	
  	val t1 = mixedLayer(Image,Kernel1,imageSize,kernelSize1,ReLu,avgpool,Size)
  	val t2 = mixedLayer(Image,Kernel2,imageSize,kernelSize2,ReLu,avgpool,Size)
 		val t3 = activationLayer((p:Double)=>p*w1,t1)
 		val t4 = activationLayer((p:Double)=>p*w2 + b,t2)
 		val t5 = add(t3,t4)
 		val t6 = mixedLayer(t5,Kernel3,List(t5.size,t5.tail.head.size),kernelSize3,l_ReLu,maxpool,Size)
		val t7 = normalise(t6)
		t7
  }         
}