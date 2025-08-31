clear all;
close all;
Input_path="C:\Users\Yef0\Desktop\study\data_unprocessed\Fertilizing_and_Mowing\ND_AFD_D_23.xlsx";
Input_path0="C:\Users\Yef0\Desktop\study\data_unprocessed\Fertilizing_and_Mowing\ND_AFD_D.xlsx";
I=xlsread(Input_path);
I0=xlsread(Input_path0);
%I(:,12:16)=I(:,12:16)-I0(:,12:16);

x=I(:,8);
y=I(:,11);
z=I(:,13);

F = scatteredInterpolant(x, y, z,'nearest');
%F = scatteredInterpolant(x, y, z,'linear');
%F = scatteredInterpolant(x, y, z,'natural');


% 定义目标网格
xi = linspace(min(x), max(x), 25); % x 轴的网格点
yi = linspace(min(y), max(y), 25); % y 轴的网格点
[Xi, Yi] = meshgrid(xi, yi); % 生成网格
Zi = F(Xi, Yi);
%plot3(Xi,Yi,Zi,'o')
%surf(xi,yi,Zi);

% 生成高斯滤波器（σ控制平滑强度，k为滤波器大小）
sigma = 1.5; 
h = fspecial('gaussian', [5 5], sigma);
smoothed_data = imfilter(Zi, h, 'replicate');

% 或直接使用imgaussfilt函数
smoothed_data = imgaussfilt(Zi, sigma, 'FilterSize', 5);

Zi=smoothed_data;
figure; % 创建新图形窗口
contourf(xi, yi, Zi, 12); % 使用 20 条等高线并填充颜色

% 添加颜色条
colorbar;

% 设置颜色映射
colormap(othercolor('RdBu10')); % 使用 "jet" 颜色映射（也可以选择其他颜色映射，如 'parula', 'hot', 'cool' 等）

% 设置标题和坐标轴标签
%title('', 'FontSize', 14, 'FontWeight', 'bold'); % 添加标题并设置字体大小和加粗
xlabel('ND', 'FontSize', 14); % 添加 X 轴标签并设置字体大小
ylabel('AFD', 'FontSize', 14); % 添加 Y 轴标签并设置字体大小

% 调整颜色条标签
h = colorbar; % 获取颜色条句柄
h.Label.String = 'Shannon-Wiener Diversity Index'; % 设置颜色条标签
%h.Label.String = "Pielou's Evenness Index"; % 设置颜色条标签
%h.Label.String = 'Berger-Parker Dominance Index'; % 设置颜色条标签
h.Label.FontSize = 14; % 设置颜色条标签字体大小

% 添加网格线
grid on; % 打开网格线
grid minor; % 显示次级网格线
ax = gca;
% 设置坐标轴范围和刻度

xticks(linspace(min(xi), max(xi), 5)); % 设置 X 轴刻度
yticks(linspace(min(yi), max(yi), 5)); % 设置 Y 轴刻度

xtickformat('%.1f'); % 设置 X 轴刻度标签格式
ytickformat('%.1f'); % 设置 Y 轴刻度标签格式

ax.XAxis.MinorTickValues = linspace(min(xi), max(xi), 21); % 次级网格线与主网格线对齐
ax.YAxis.MinorTickValues = linspace(min(yi), max(yi), 21); % 次级网格线与主网格线对

% 设置图形背景颜色
%set(gcf, 'Color', [0.95, 0.95, 0.95]); % 设置背景颜色为浅灰色

% 设置图形边框
box on; % 添加边框


