using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using LSystems;
using System.Windows.Forms;

namespace LSystems.UI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        OpenFileDialog fileDialog = new OpenFileDialog();

        public MainWindow()
        {
            InitializeComponent();
        }

        Dictionary<char, Microsoft.FSharp.Collections.FSharpList<Expression>> rulesSet;
        string axiom = "";

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            var res = fileDialog.ShowDialog();

            if (res == System.Windows.Forms.DialogResult.OK)
            {
                rulesTextBox.Text = System.IO.File.ReadAllText(fileDialog.FileName);
            }
        }

        LinkedList<Line> _lines = new LinkedList<Line>();

        private void Button_Click_1(object sender, RoutedEventArgs e)
        {
            var r = Utils.parseRules(rulesTextBox.Text.Replace("\r","").Split('\n'));
            rulesSet = r.Item1;
            axiom = r.Item2;
            foreach (var l in _lines)
            {
                mainCanvas.Children.Remove(l);
            }
            _lines.Clear();
            
            var lineParam = new LineParams(new Tuple<int, int, int>(0,0,0), new Tuple<double, double>(0,0), 0, 1, 5, true);
            var changeParam = new TransformParams(new Tuple<int, int, int>(0, 0, 0), new Tuple<double, double>(1,0), 1, 90);
            var appl = Utils.applyN(rulesSet, int.Parse(iterationsBox.Text), axiom);
            var lines = Utils.compile(rulesSet, lineParam, changeParam, appl);

            Func<int, byte> cut = x => (byte)(x > 255 ? 255 : x < 0 ? 0 : x);

            foreach(var l in lines)
            {
                var l1 = l.Item1;
                var l2 = l.Item2;

                if (!l1.Visible) continue;

                var line = new Line
                {
                    X1 =l1.Position.Item1,
                    Y1 =l1.Position.Item2,
                    X2 =l2.Position.Item1,
                    Y2 =l2.Position.Item2,
                    Stroke =new SolidColorBrush(Color.FromRgb(cut(l1.Color.Item1), cut(l1.Color.Item2), cut(l1.Color.Item3))),
                    StrokeThickness =l1.Width,
                    StrokeEndLineCap =PenLineCap.Round
                };
                mainCanvas.Children.Add(line);
                _lines.AddLast(line);
            }
        }
    }
}
