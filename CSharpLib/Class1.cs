namespace CSharpLib
{
    public class Class1
    {
        public void ProcessDu(SmallLibWithDU.MyDU x)
        {
            // Outside of "IsB" tester, field can be null
            string s = x.stringField;
            if(x.IsB)
            {
                s = x.stringField;
            }
        }
    }
}
